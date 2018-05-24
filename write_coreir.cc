// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

#include "kernel/yosys.h"
#include "kernel/sigtools.h"

#include "algorithm.h"

#include "coreir.h"
#include "coreir/libs/rtlil.h"

#include <string>
#include <map>
#include <set>

using namespace CoreIR;
using namespace std;

#define HIGH_IMPEDANCE_BIT 3
#define UNKNOWN_VALUE_BIT 2

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

void print_cell_info(RTLIL::Cell* const cell);

bool isRTLILBinop(const std::string& cellTp) {
  if (cellTp[0] == '$') {
    string opName = cellTp.substr(1, cellTp.size());
    vector<string> rtlilBinops{"and", "or", "xor", "xnor", "shl", "shr", "sshl", "sshr", "logic_and", "logic_or", "eqx", "nex", "lt", "le", "eq", "ne", "ge", "gt", "add", "sub", "mul", "div", "mod", "pow"};

    for (auto& e : rtlilBinops) {
      if (e == opName) {
        return true;
      }
    }
  }

  return false;
}

bool isRTLILUnop(const std::string& cellTp) {
  if (cellTp[0] == '$') {
    string opName = cellTp.substr(1, cellTp.size());

  vector<string> rtlilUnops{"not", "pos", "neg", "reduce_and", "reduce_or", "reduce_xor", "reduce_xnor", "reduce_bool", "logic_not"};

    for (auto& e : rtlilUnops) {
      if (e == opName) {
        return true;
      }
    }
  }

  return false;
}

int getIntParam(Cell* const cell, const std::string& str) {
  auto param = cell->parameters.find(str);

  if (param == std::end(cell->parameters)) {
    cout << "Cannot find parameter " << str << " in " << endl;
    print_cell_info(cell);
    assert(false);
  }

  return param->second.as_int();
}

BitVec getBitVecParam(Cell* const cell, const std::string& str) {
  auto param = cell->parameters.find(str);

  if (param == std::end(cell->parameters)) {
    cout << "Cannot find parameter " << str << " in " << endl;
    print_cell_info(cell);
    assert(false);
  }

  BitVec bv(param->second.bits.size(), 0);
  for (int i = 0; i < ((int) bv.bitLength()); i++) {
    bv.set(i, param->second.bits[i]);
  }

  return bv;
  //return param->second.as_int();
}

std::string coreirSafeName(const std::string cellName) {
  string instName = "";
  for (uint i = 0; i < cellName.size(); i++) {
    if (cellName[i] == '$') {
      instName += "__DOLLAR__";
    } else if (cellName[i] == ':') {
      instName += "__COLON__";
    } else if (cellName[i] == '.') {
      instName += "__DOT__";
    } else if (cellName[i] == '\\') {
      instName += "__BACKSLASH__";
    } else if (cellName[i] == '=') {
      instName += "__EQUALS__";
    } else if (cellName[i] == '[') {
      instName += "__LEFT_BRACKET__";
    } else if (cellName[i] == ']') {
      instName += "__RIGHT_BRACKET__";
    } else if (cellName[i] == '/') {
      instName += "__FORWARD_SLASH__";
    } else {
      instName += cellName[i];
    }

  }

  return instName;
}


void print_cell_info(RTLIL::Cell* const cell) {

  string cellTp = RTLIL::id2cstr(cell->type);
  string cellName = RTLIL::id2cstr(cell->name);
        
  cout << cellName << endl;

  cout << "Cell: " <<
    RTLIL::id2cstr(cell->name) << " : " << 
    RTLIL::id2cstr(cell->type) << endl;

  for (auto& param : cell->parameters) {
    cout << "\tParam: " <<
      RTLIL::id2cstr(param.first) << 
      param.second.as_string().c_str() << endl;
  }

  for (auto& conn : cell->connections()) {
    cout << "\tPort: " << id2cstr(conn.first) << " : " << conn.second.size() << endl;
  }

}

struct CoreIRWriter {
  Context* c;
  Namespace* ns;
  RTLIL::Design * const design;
  
  //This is for internal modules/generators
  std::map<RTLIL::IdString, CoreIR::Module*> modMap;

  //This is for external ones
  std::map<RTLIL::Cell*, CoreIR::Module*> externalMap;
  
  std::map<RTLIL::IdString,std::string> gennameMap;


  CoreIRWriter(Context* c, Namespace* ns, RTLIL::Design* design) : c(c), ns(ns), design(design) {}

  CoreIR::Module* getModuleRef(RTLIL::Cell* cell) {
    if (externalMap.count(cell)) {
      return externalMap[cell];
    }
    if (modMap.count(cell->type)) {
      return modMap[cell->type];
    }
    cout << "Error cannot find module!" << endl;
    assert(0);
    return nullptr;
  }

  //This is for adding internal modules
  void addModule(RTLIL::IdString rmodName, dict<RTLIL::IdString, RTLIL::Const> rparams, set<RTLIL::IdString>& completed) {
    RTLIL::Module* rmod = design->module(rmodName);
    string cmodName = id2cstr(rmod->name);
    if (gennameMap.count(rmod->name)) {
      cmodName = gennameMap[rmod->name];
    }

    cout << "{ adding Module " << cmodName << endl;
    bool is_generator = rparams.size()>0;
    if (!is_generator && rmod->avail_parameters.size()>0) {
      cout << "} abstract gen" << endl;
      return;
    }
    
    //Check parameters are same as avail_params
    CoreIR::Params cparams;
    CoreIR::Values cargs;
    for (auto ppair : rparams) {
      assert(rmod->avail_parameters.count(ppair.first)>0);
      //TODO assume all params are int for now
      cargs[id2cstr(ppair.first)] = CoreIR::Const::make(c,ppair.second.as_int());
      cparams[id2cstr(ppair.first)] = c->Int(); 
    }
    cout << " with args = " << CoreIR::toString(cargs) << endl;
    for (auto aparam : rmod->avail_parameters) {
      cout << "aparam: " << id2cstr(aparam) << endl;
      assert(rparams.count(aparam)>0);
    }

    if (completed.count(rmodName)) {
      CoreIR::Module* cmod = modMap.at(rmodName);
      if (is_generator) {
        assert(cmod->isGenerated());
        assert(cmod->getGenerator()->getName()==cmodName);
        assert(cmod->getGenArgs()==cargs);
        assert(ns->getGenerator(cmodName) == cmod->getGenerator());
      }
      else {
        assert(cmod->getName()==cmodName);
        assert(ns->getModule(cmodName)==cmod);
      }
      return;
    }
    vector<pair<string, Type*> > recordParams;

    //cout << "Wires" << endl;
    for (auto &port_name : rmod->ports) {

      RTLIL::Wire *wire = rmod->wires_[port_name];
      if (wire->port_output) {

        Type* bitTp = c->Bit();
        if (wire->port_input) {
          cout << id2cstr(wire->name) << " is an inout" << endl;
          bitTp = c->BitInOut();
        }

        if (wire->width > 1) {
          recordParams.push_back({id2cstr(wire->name), c->Array(wire->width, c->Bit())});
        } else {
          recordParams.push_back({id2cstr(wire->name), bitTp});
        }
        continue;
      }
      if (wire->port_input) {
        assert(!wire->port_output);

        if (wire->width > 1) {
          recordParams.push_back({id2cstr(wire->name), c->Array(wire->width, c->BitIn())});
        } else {
          recordParams.push_back({id2cstr(wire->name), c->BitIn()});
        }
      }
    }


    RecordType* modType = c->Record(recordParams);

    CoreIR::Module* cmod;
    if (is_generator) {
      //This is a generator 
      Generator* gen;
      if (!ns->hasGenerator(cmodName)) {
        gen = ns->newGeneratorDecl(cmodName,TypeGenImplicit::make(ns,cmodName,cparams),cparams);
      }
      else {
        gen = ns->getGenerator(cmodName);
        assert(gen->getGenParams()==cparams);
      }
      cmod = gen->getModule(cargs,modType);
    }
    else {
      cmod = ns->newModuleDecl(cmodName,modType);
    }
    modMap[rmodName] = cmod;
    completed.insert(rmodName);
  
    //Now go through all cells, and add all the 'generated' modules
    for (auto& cell_iter : rmod->cells_) {
      Cell* cell = cell_iter.second;
      if (cell->type[0] == '$') {
        cout << "not processing " << id2cstr(cell->type) << endl;
        continue;
      }
      bool is_generator = cell->parameters.size() > 0;
      cout << "celltype=" << RTLIL::id2cstr(cell->type) << endl;
      bool is_external = false;
      string ext_namespace;
      for (auto apair : cell->attributes) {
        if (RTLIL::id2cstr(apair.first)==string("namespace")) {
          ext_namespace = apair.second.decode_string();
          is_external = true;
          cout << "Found cell with external namespace! " << ext_namespace << endl;
        }
      }

      //If cell is external, get the module and save in externalMap
      if (is_external) {
        //load the external lib first
        string modRefName = id2cstr(cell->type);
        Namespace* nsRef = c->getLibraryManager()->loadLib(ext_namespace);
        if (is_generator) {
          CoreIR::Values genargs;
          for (auto ppair : cell->parameters) {
            string argname = id2cstr(ppair.first);
            genargs[argname] = CoreIR::Const::make(c,ppair.second.as_int());
          }
          externalMap[cell] = nsRef->getGenerator(modRefName)->getModule(genargs);
        }
        else {
          externalMap[cell] = nsRef->getModule(modRefName);
        }
      }
      else if (is_generator) { //cell is internal and a generator
        RTLIL::Module* containerMod = cell->module;
        Design* rtd = containerMod->design;

        RTLIL::Module* rtmod = rtd->modules_[cell->type];
        cout << "RTMOD = " << id2cstr(rtmod->name) << endl;

        RTLIL::IdString rgenName = rtmod->derive(rtmod->design, cell->parameters);
        gennameMap[rgenName] = id2cstr(cell->type);
        cell->type = rgenName;
        this->addModule(rgenName,cell->parameters,completed);
      }
    } //end cell for loop
    cout << "} adding Module " << cmodName << endl;
  }

  void buildModuleMap() {

    vector<RTLIL::IdString> work;
    for (auto &it : design->modules_) {
      assert(it.first == it.second->name);
      work.push_back(it.first);
    }
    std::set<RTLIL::IdString> completed;
    for (auto mod : work) {
      this->addModule(mod,{},completed);
    }
  }


  map<Cell*, Instance*> buildInstanceMap(RTLIL::Module* const rmod,
                                         CoreIR::ModuleDef* const def) {

    map<Cell*, Instance*> instMap;
    for (auto& cell_iter : rmod->cells_) {
      Cell* cell = cell_iter.second;

      string cellTp = RTLIL::id2cstr(cell->type);
      string cellName = RTLIL::id2cstr(cell->name);
      string instName = coreirSafeName(cellName);

      if (isRTLILBinop(cellTp)) {
        string opName = cellTp.substr(1, cellTp.size());
        //cout << "opName = " << opName << endl;

        int widthA = getIntParam(cell, "\\A_WIDTH");
        int widthB = getIntParam(cell, "\\B_WIDTH");
        int widthY = getIntParam(cell, "\\Y_WIDTH");
        int signedA = getIntParam(cell, "\\A_SIGNED");
        int signedB = getIntParam(cell, "\\B_SIGNED");

        auto inst = def->addInstance(instName, "rtlil." + opName,
                                     {{"A_SIGNED", CoreIR::Const::make(c, (bool) signedA)},
                                         {"B_SIGNED", CoreIR::Const::make(c, (bool) signedB)},
                                           {"A_WIDTH", CoreIR::Const::make(c, widthA)},
                                             {"B_WIDTH", CoreIR::Const::make(c, widthB)},
                                               {"Y_WIDTH", CoreIR::Const::make(c, widthY)}});

        instMap[cell] = inst;
        
        
      } else if (isRTLILUnop(cellTp)) {
        string opName = cellTp.substr(1, cellTp.size());
        //cout << "opName = " << opName << endl;

        int widthA = getIntParam(cell, "\\A_WIDTH");
        int widthY = getIntParam(cell, "\\Y_WIDTH");
        int signedA = getIntParam(cell, "\\A_SIGNED");

        auto inst = def->addInstance(instName, "rtlil." + opName,
                                     {{"A_SIGNED", CoreIR::Const::make(c, (bool) signedA)},
                                           {"A_WIDTH", CoreIR::Const::make(c, widthA)},
                                               {"Y_WIDTH", CoreIR::Const::make(c, widthY)}});

        instMap[cell] = inst;
        
      } else if (cellTp == "$mux") {
        string opName = cellTp.substr(1, cellTp.size());
        //cout << "opName = " << opName << endl;

        int width = getIntParam(cell, "\\WIDTH");

        // Change to just mux?
        auto inst = def->addInstance(instName, "rtlil.rtMux",
                                     {{"WIDTH", CoreIR::Const::make(c, width)}});

        instMap[cell] = inst;
        
      } else if (cellTp == "$dlatch") {


        int width = getIntParam(cell, "\\WIDTH");
        int polarity = getIntParam(cell, "\\EN_POLARITY");

        auto inst = def->addInstance(instName, "rtlil.dlatch",
                                     {{"WIDTH", CoreIR::Const::make(c, width)},
                                         {"EN_POLARITY", CoreIR::Const::make(c, (bool) polarity)}});

        instMap[cell] = inst;

      } else if (cellTp == "$dff") {


        int width = getIntParam(cell, "\\WIDTH");
        int polarity = getIntParam(cell, "\\CLK_POLARITY");

        auto inst = def->addInstance(instName, "rtlil.dff",
                                     {{"WIDTH", CoreIR::Const::make(c, width)},
                                         {"CLK_POLARITY", CoreIR::Const::make(c, (bool) polarity)}});

        instMap[cell] = inst;

      } else if (cellTp == "$dffsr") {


        int width = getIntParam(cell, "\\WIDTH");
        int clk_polarity = getIntParam(cell, "\\CLK_POLARITY");
        int clr_polarity = getIntParam(cell, "\\CLR_POLARITY");
        int set_polarity = getIntParam(cell, "\\SET_POLARITY");

        auto inst = def->addInstance(instName, "rtlil.dffsr",
                                     {{"WIDTH", CoreIR::Const::make(c, width)},
                                         {"CLK_POLARITY", CoreIR::Const::make(c, (bool) clk_polarity)},
                                           {"CLR_POLARITY", CoreIR::Const::make(c, (bool) clr_polarity)},
                                             {"SET_POLARITY", CoreIR::Const::make(c, (bool) set_polarity)}});

        instMap[cell] = inst;

      } else if (cellTp == "$shiftx") {

        int a_width = getIntParam(cell, "\\A_WIDTH");
        int a_signed = getIntParam(cell, "\\A_SIGNED");
        int b_width = getIntParam(cell, "\\B_WIDTH");
        int b_signed = getIntParam(cell, "\\B_SIGNED");
        int y_width = getIntParam(cell, "\\Y_WIDTH");

        auto inst = def->addInstance(instName, "rtlil.shiftx",
                                     {{"A_WIDTH", CoreIR::Const::make(c, a_width)},
                                         {"B_WIDTH", CoreIR::Const::make(c, b_width)},
                                           {"Y_WIDTH", CoreIR::Const::make(c, y_width)},
                                             {"A_SIGNED", CoreIR::Const::make(c, (bool) a_signed)},
                                               {"B_SIGNED", CoreIR::Const::make(c, (bool) b_signed)}});

        instMap[cell] = inst;

      } else if (cellTp == "$adff") {


        int width = getIntParam(cell, "\\WIDTH");
        int polarity = getIntParam(cell, "\\CLK_POLARITY");
        int rstPolarity = getIntParam(cell, "\\ARST_POLARITY");
        //int rstValue = getIntParam(cell, "\\ARST_VALUE");
        BitVec rstValue = getBitVecParam(cell, "\\ARST_VALUE");

        auto inst = def->addInstance(instName, "rtlil.adff",
                                     {{"WIDTH", CoreIR::Const::make(c, width)},
                                         {"CLK_POLARITY", CoreIR::Const::make(c, (bool) polarity)},
                                           {"ARST_POLARITY", CoreIR::Const::make(c, (bool) rstPolarity)}},
                                     {{"init", CoreIR::Const::make(c, rstValue)}});

        instMap[cell] = inst;

      } else if (cellTp == "$mem") {

        int width = getIntParam(cell, "\\WIDTH");
        int size = getIntParam(cell, "\\SIZE");

        auto inst = def->addInstance(coreirSafeName(cellName),
                                     "rtlil.memory",
                                     {{"WIDTH", CoreIR::Const::make(c, width)},
                                         {"SIZE", CoreIR::Const::make(c, size)}});

        instMap[cell] = inst;
      } else {
        
        cout << "CellType=" << id2cstr(cell->type) << endl;
        CoreIR::Module* modRef = this->getModuleRef(cell);
        auto inst = def->addInstance(instName, modRef);
        instMap[cell] = inst;
      }
    }

    return instMap;
  }

  void printSigSigInfo(RTLIL::SigSig conn) {
        RTLIL::SigSpec fst = conn.first;
        RTLIL::SigSpec snd = conn.second;
            
        cout << "\tSigSpec fst" << endl;
        cout << "\t\tis wire  = " << fst.is_wire() << endl;
        cout << "\t\tis chunk = " << fst.is_chunk() << endl;

        cout << "\tSigSpec snd" << endl;
        cout << "\t\tis wire  = " << snd.is_wire() << endl;
        cout << "\t\tis chunk = " << snd.is_chunk() << endl;
  }

  std::string coreirPort(Cell* const cell,
                         const std::string& portName) {
    string cellTp = id2cstr(cell->type);

    return portName;
  }

  bool isBitType(CoreIR::Type* const tp) {
    if ((tp->getKind() == Type::TK_Bit) ||
        (tp->getKind() == Type::TK_BitIn) ||
        (tp->getKind() == Type::TK_BitInOut)) {
      return true;
    }

    return false;
  }

  CoreIR::Select* instanceSelect(Cell* const cell,
                                 const std::string& portName,
                                 const int wireOffset,
                                 map<Cell*, CoreIR::Instance*>& instMap) {

    assert(cell != nullptr);

    Instance* inst = instMap[cell];

    if (inst == nullptr) {
      cout << "Error: Instance map does not contain " << endl;
      print_cell_info(cell);
      assert(false);
    }

    string coreIRPort = coreirPort(cell, portName);

    //cout << "Instance sel " << coreIRPort << endl;
    auto port = inst->sel(coreIRPort);

    if (isBitType(port->getType())) {
      if (wireOffset != 0) {

        cout << "Error: Trying to select bit " << wireOffset << " from single bit port: " << port->toString() << ", portName = " << portName << endl;
        cout << "\tRTLIL Cell" << endl;
        print_cell_info(cell);
        cout << "\tRTLIL Port Name = " << portName << endl;
        cout << "\tWire offset     = " << wireOffset << endl;
        assert(false);
      }

      return port;
    }

    auto portBit = port->sel(wireOffset);

    return portBit;
  }

  void printModuleInfo(RTLIL::Module* const rmod) {
    cout << "########## Module info for module: " << id2cstr(rmod->name) << endl;

    SigMap sigmap(rmod);
    dict<SigBit, Cell*> sigbit_to_driver_index;
    dict<SigBit, string> sigbit_to_driver_port_index;
    for (auto cell : rmod->cells()) {
      for (auto conn : cell->connections()) {
        if (cell->output(conn.first)) {
          for (auto bit : sigmap(conn.second)) {
            sigbit_to_driver_index[bit] = cell;
            sigbit_to_driver_port_index[bit] = id2cstr(conn.first);
          }
        }
      }
    }

    dict<SigBit, Cell*> sigbit_to_receiver_index;
    dict<SigBit, string> sigbit_to_receiver_port_index;
    for (auto cell : rmod->cells()) {
      for (auto conn : cell->connections()) {
        if (cell->input(conn.first)) {
          for (auto bit : sigmap(conn.second)) {
            sigbit_to_receiver_index[bit] = cell;
            sigbit_to_receiver_port_index[bit] = id2cstr(conn.first);
          }
        }
      }
    }

    cout << "----All cell connections" << endl;
    for (auto cell : rmod->cells()) {
      cout << "\tConnections for " << id2cstr(cell->name) << endl;
      for (auto conn : cell->connections()) {
        cout << "\t\t" << id2cstr(conn.first) << " --> " << id2cstr(conn.second.as_wire()->name) << endl;
      }
    }
    
    cout << "All wires" << endl;
    for (auto wire : rmod->wires()) {
      cout << "\t" << id2cstr(wire->name) << endl;
      int i = 0;
      for (auto& bit : sigmap(wire)) {

        // cout << "\t\tBit wire = " << id2cstr(bit.wire->name) << endl;
        // cout << "\t\tDrivers" << endl;
        Cell* driverCell = sigbit_to_driver_index[bit];

        if (driverCell != nullptr) {
          cout << "\t\t" << id2cstr(wire->name) << " " << bit.offset << " = " << id2cstr(sigbit_to_driver_index[bit]->name) << "." << sigbit_to_driver_port_index[bit] << endl;
        } else {
          cout << "\t\t" << id2cstr(wire->name) << " " << bit.offset << " = NULL;" << endl;
        }

        cout << "\t\tReceivers" << endl;
        Cell* receiverCell = sigbit_to_receiver_index[bit];

        if (receiverCell != nullptr) {
          cout << "\t\t" << id2cstr(wire->name) << " " << bit.offset << " = " << id2cstr(sigbit_to_receiver_index[bit]->name) << "." << sigbit_to_receiver_port_index[bit] << endl;
        } else {
          cout << "\t\t" << id2cstr(wire->name) << " " << bit.offset << " = NULL;" << endl;
        }
        
        i++;
      }
    }

    cout << "All wire <-> wire connections" << endl;      

    for (auto conn : rmod->connections()) {
      SigSpec l = conn.first;
      SigSpec r = conn.second;

      if (l.is_wire() && r.is_wire()) {
        cout << "( " << id2cstr(l.as_wire()->name) << ", " << id2cstr(r.as_wire()->name) << " )" << endl;
        //cout << "\tSigSpec size = " << l.size() << ", is_wire = " << l.is_wire() << ", is chunk = " << l.is_chunk() << endl;
      }
          
    }

  }

  void
  buildSelectMap(RTLIL::Module* const rmod,
                 map<Cell*, CoreIR::Instance*>& instMap,
                 ModuleDef* const def) {

    cout << "########## Module info for module: " << id2cstr(rmod->name) << endl;

    SigMap sigmap(rmod);

    dict<SigBit, Cell*> sigbit_to_driver_index;
    dict<SigBit, string> sigbit_to_driver_port_index;
    dict<SigBit, int> sigbit_to_driver_offset;

    for (auto cell : rmod->cells()) {
      for (auto conn : cell->connections()) {
        if (cell->output(conn.first)) {

          int i = 0;
          for (auto bit : sigmap(conn.second)) {
            sigbit_to_driver_index[bit] = cell;
            sigbit_to_driver_port_index[bit] = id2cstr(conn.first);
            sigbit_to_driver_offset[bit] = i;

            i++;
          }
        }
      }
    }

    cout << "sigbit_to_driver" << endl;
    for (auto sigbitR : sigbit_to_driver_index) {
      SigBit sigbit = sigbitR.first;
      //Cell* driver = sigbitR.second;

      // NOTE: I dont think this should ever happen. Constants are not driven
      if (sigbit.wire == nullptr) {
        //cout << "data = " << id2cstr(sigbit.wire->name) << " driven by " << driver << endl;
      } else {
        //cout << id2cstr(sigbit.wire->name) << " [ " << sigbit.offset << " ] " << "driven by " << id2cstr(driver->name) << endl;
      }
    }

    for (auto wire : rmod->wires()) {
      if (wire->port_input && !wire->port_output) {
        int i = 0;
        for (auto bit : sigmap(wire)) {
          sigbit_to_driver_port_index[bit] = id2cstr(wire->name);
          sigbit_to_driver_offset[bit] = i;
          i++;
        }
      }
    }

    // Build inout port to port cast map
    dict<string, Instance*> inouts_to_casts;
    //dict<string, Instance*> inouts_to_out_casts;
    for (auto wire : rmod->wires()) {
      if (wire->port_input && wire->port_output) {

        auto in_out_cast = def->addInstance(string(id2cstr(wire->name)) + "_in_cast",
                                            "rtlil.padIO",
                                            {{"WIDTH", CoreIR::Const::make(c, wire->width)}});
        Select* port = def->sel("self")->sel(id2cstr(wire->name));
        if (isBitType(port->getType())) {
          def->connect(in_out_cast->sel("INOUT_PORT")->sel(0), port);
        } else {
          assert(false);
        }

        inouts_to_casts[id2cstr(wire->name)] = in_out_cast;

      }
    }

    cout << "Adding input to driver connections" << endl;
    // Add connections from inputs to drivers
    for (auto cell : rmod->cells()) {
      //cout << "Cell = " << id2cstr(cell->name) << endl;
      for (auto conn : cell->connections()) {
        //cout << "Conn = " << id2cstr(conn.first) << endl;
        if (cell->input(conn.first)) {

          if (cell->output(conn.first)) {
            //cout << "Cell " << id2cstr(cell->name) << "." << id2cstr(conn.first) << " is an io port" << endl;

            int i = 0;
            for (auto bit : sigmap(conn.second)) {
              assert(bit.wire != nullptr);
              //cout << "bit.wire " << bit.offset << " = " << id2cstr(bit.wire->name) << endl;
              assert(bit.wire->port_input && bit.wire->port_output);

              Instance* port = inouts_to_casts[id2cstr(bit.wire->name)];
              assert(port != nullptr);
              Select* p = port->sel("INOUT_DRIVER_PORT")->sel(bit.offset);

              //cout << "Connecting to " << p->toString() << endl;

              // From driver to the current bit
              Select* to = instanceSelect(cell,
                                          id2cstr(conn.first),
                                          i,
                                          instMap);
              //cout << "to = " << to->toString() << endl;
              
              i++;

              def->connect(to, p);
            }
          } else {
            // Not sure if I really need this index variable or if the index is
            // stored somewhere else
            int i = 0;
            for (auto bit : sigmap(conn.second)) {

              if (bit.wire != nullptr) {

                Cell* driver = sigbit_to_driver_index[bit];

                if (sigbit_to_driver_port_index.find(bit) ==
                    end(sigbit_to_driver_port_index)) {
                  //cout << "Bit " << bit.offset << " for wire " << id2cstr(bit.wire->name) << " has no port?" << endl;
                  continue;
                }

                string port = sigbit_to_driver_port_index[bit];
                //cout << "port = " << port << endl;

                // From driver to the current bit
                Select* to = instanceSelect(cell,
                                            id2cstr(conn.first),
                                            i,
                                            instMap);
                //cout << "to = " << to->toString() << endl;

                Select* from = nullptr;
                if (driver != nullptr) {
                  from = instanceSelect(driver, port, sigbit_to_driver_offset[bit], /*bit.offset*/ instMap);
                } else {

                  //cout << "Selecting " << port << " off of select" << endl;
                  from = def->sel("self")->sel(port);
                  if (!isBitType(from->getType())) {
                    // The sigbit
                    assert(sigbit_to_driver_offset.find(bit) !=
                           end(sigbit_to_driver_offset));

                    from = from->sel(sigbit_to_driver_offset[bit]);
                  } else {
                    assert(bit.offset == 0);
                  }
                }

                assert(from != nullptr);

                // cout << "from = " << from->toString() << endl;
                // cout << "Connecting " << from->toString() << " to " << to->toString() <<                endl;
                def->connect(from, to);
              } else {
                //cout << "Wire is null, bit state = " << bit.data << endl;

                assert((bit.data == 0) || (bit.data == 1) ||
                       (bit.data == HIGH_IMPEDANCE_BIT) ||
                       (bit.data == UNKNOWN_VALUE_BIT));
                // Q: How do I know what offset the bit maps to in a wire if the bit
                // offset field is not set? For now use index variable
              
                // From driver to the current bit
                Select* to = instanceSelect(cell,
                                            id2cstr(conn.first),
                                            i,
                                            instMap);

                Instance* const_inst;
                Select* from = nullptr;
                if ((bit.data == 0) ||
                    (bit.data == 1)) {
                  const_inst =
                    def->addInstance(coreirSafeName(to->toString() + "_bit_const_" + to_string(i)),
                                     "corebit.const",
                                     {{"value", CoreIR::Const::make(c, bit.data == 1 ? true : false)}});
                  from = const_inst->sel("out");
                } else if (bit.data == HIGH_IMPEDANCE_BIT) {
                  const_inst =
                    def->addInstance(coreirSafeName(to->toString() + "_high_impedance_" + to_string(i)),
                                     "rtlil.highImpedanceBit");

                  from = const_inst->sel("OUT");
                } else {
                  assert(bit.data == UNKNOWN_VALUE_BIT);

                  const_inst =
                    def->addInstance(coreirSafeName(to->toString() + "_unknown_value_" + to_string(i)),
                                     "rtlil.unknownBit");

                  from = const_inst->sel("OUT");

                }

                assert(from != nullptr);

                def->connect(from, to);

              }
              i++;
            }
          }
        }
      }
    }

    cout << "Adding output connections to wires" << endl;
    for (auto wire : rmod->wires()) {

      // // Handle inout wires separately?
      if (wire->port_output && wire->port_input) {

        Instance* cast = inouts_to_casts[id2cstr(wire->name)];
        assert(cast != nullptr);

        int i = 0;
        for (auto bit : sigmap(wire)) {
          cout << "Bit wire = " << id2cstr(bit.wire->name) << ", offset = " << bit.offset << endl;
          assert(bit.wire != nullptr);

          // // Possible solution: Check if the ports being connected are inouts?
          // if (bit.wire->port_input && bit.wire->port_output) {
          //   // This connection has already been wired up unless it is a connection
          //   //continue;

          //   if (sigbit_to_driver_index.find(bit) == end(sigbit_to_driver_index)) {
          //     cout << "Connecting 2 inout ports" << endl;
          //     continue;
          //   }
          // }

          assert(sigbit_to_driver_port_index.find(bit) !=
                 end(sigbit_to_driver_port_index));
          
          Cell* driver = sigbit_to_driver_index[bit];

          assert(driver != nullptr);
          cout << "driver = " << id2cstr(driver->name) << endl;
          string port = sigbit_to_driver_port_index[bit];
          //cout << "port = " << port << endl;

          int offset = sigbit_to_driver_offset[bit];

          Select* from = nullptr;
          if (driver != nullptr) {
            //cout << "Driver = " << id2cstr(driver->name) << endl;
            from = instanceSelect(driver, port, offset, instMap);
            //cout << "Done selecting" << endl;
          } else {

            from = def->sel("self")->sel(port);
            if (!isBitType(from->getType())) {
              from = from->sel(offset);
            } else {
              assert(bit.offset == 0);
            }
          }
          
          assert(from != nullptr);

          cout << "from = " <<  from->toString() << endl;
          cout << "from->getDir() = " << from->getType()->getDir() << endl;
          if (from->getType()->getDir() == CoreIR::Type::DK_InOut) {
            continue;
          }

          // Maybe move this outside the bit loop
          Select* to = cast->sel("IN_PORT"); //cast<Select>(def->sel("self")->sel(id2cstr(wire->name)));
          if (!isBitType(to->getType())) {
            to = to->sel(i);
          }

          //cout << "Connecting " << from->toString() << " to " << to->toString() << " : " << to->getType()->toString() << endl;

          def->connect(from, to);
          
        }

        //assert(false);

      } else if (wire->port_output) {

        cout << "Wiring up inputs to output port " << id2cstr(wire->name) << endl;
        int i = 0;
        for (auto bit : sigmap(wire)) {

          if (bit.wire != nullptr) {
            //cout << "Bit wire = " << id2cstr(bit.wire->name) << ", offset = " << bit.offset << endl;

            if (bit.wire->port_input && bit.wire->port_output) {
              // Maybe move this outside the bit loop
              Select* to = cast<Select>(def->sel("self")->sel(id2cstr(wire->name)));
              if (!isBitType(to->getType())) {
                to = to->sel(i);
              }

              Select* from = inouts_to_casts[id2cstr(bit.wire->name)]->sel("OUT_PORT");
              if (!isBitType(from->getType())) {
                from = from->sel(0);
              } else {
                assert(bit.offset == 0);
              }

              assert(from != nullptr);

              //cout << "InOut Connecting " << from->toString() << " to " << to->toString() << " : " << to->getType()->toString() << endl;

              def->connect(from, to);
            } else if (sigbit_to_driver_port_index.find(bit) !=
                end(sigbit_to_driver_port_index)) {
          
              Cell* driver = sigbit_to_driver_index[bit];
              string port = sigbit_to_driver_port_index[bit];
              //cout << "port = " << port << endl;
              int offset = sigbit_to_driver_offset[bit];

              Select* from = nullptr;
              if (driver != nullptr) {
                //cout << "Driver = " << id2cstr(driver->name) << endl;
                from = instanceSelect(driver, port, offset, instMap);
                //cout << "Done selecting" << endl;
              } else {

                from = def->sel("self")->sel(port);
                if (!isBitType(from->getType())) {
                  from = from->sel(offset);
                } else {
                  assert(bit.offset == 0);
                }
              }
          
              assert(from != nullptr);

              // Maybe move this outside the bit loop
              Select* to = cast<Select>(def->sel("self")->sel(id2cstr(wire->name)));
              if (!isBitType(to->getType())) {
                to = to->sel(i);
              }

              //cout << "Connecting " << from->toString() << " to " << to->toString() << " : " << to->getType()->toString() << endl;

              def->connect(from, to);
            } else {
              cout << "ERROR: No port for " << id2cstr(wire->name) << endl;
              //assert(false);
            }
          } else {


            cout << "Wire is null, data = " << bit.data << endl;

            Select* to =
              cast<Select>(def->sel("self")->sel(id2cstr(wire->name)));

            if (!isBitType(to->getType())) {
              to = to->sel(i);
            }

            assert(to != nullptr);

            auto bitConst =
              def->addInstance(coreirSafeName(to->toString() + "$bit_const_" + to_string(i)),
                               "corebit.const",
                               {{"value", CoreIR::Const::make(c, bit.data == 1 ? true : false)}});;

            Select* from = bitConst->sel("out");

            def->connect(from, to);

            //assert(false);
          }
          i++;
        }
      }
    }

    return;

  }

  void removeRTLILTristate(CoreIR::ModuleDef* def) {

    bool removedPad = true;
    while (removedPad) {
      removedPad = false;

      for (auto instR : def->getInstances()) {
        Instance* inst = instR.second;
        if (getQualifiedOpName(*inst) == "rtlil.padIO") {

          if ((getReceiverConnections(inst->sel("OUT_PORT")).size() == 0) &&
              (getSourceConnections(inst->sel("IN_PORT")).size() == 0)) {

            Instance* pt = addPassthrough(inst, "_inline_tristate_PT");

            //cout << "pt type = " << pt->getType()->toString() << endl;
            def->disconnect(inst);

            def->connect(pt->sel("in")->sel("INOUT_PORT"),
                         pt->sel("in")->sel("INOUT_DRIVER_PORT"));
            def->removeInstance(inst);
            inlineInstance(pt);

            removedPad = true;
            break;
          } else {
            // Check that INOUT_DRIVER_PORT has no connected wireables

            // What is the correct behavior here? A: Find the mux that
            // drives the input and convert it to a tristate buffer.
            // Also: Connect the output to a tristate

            auto inputSrcs = getSourceSelects(inst->sel("IN_PORT"));
            Instance* srcMux;
            for (auto sel : inputSrcs) {
              cout << "\t" << sel->toString() << endl;
              Wireable* src = extractSource(sel);

              assert(isa<Instance>(src));

              Instance* srcInst = cast<Instance>(src);
              srcMux = srcInst;

              cout << "Source instance = " << srcInst->toString() << endl;
              cout << "Name            = " << getQualifiedOpName(*srcInst) << endl;
              assert(getQualifiedOpName(*srcInst) == "rtlil.rtMux");

              // Now: Disconnect the mux select, find the high impedance value,
              // crash if the high impedance value does not exist. If it does
              // then replace the mux with a triput

              // Then replace OUT_PORT with a triget
            }

            cout << "Source mux is " << srcMux->toString() << endl;

            Select* muxIn0 = srcMux->sel("A");
            Select* muxIn1 = srcMux->sel("B");
            Select* muxSel = srcMux->sel("S");

            vector<Select*> muxIn0Srcs =
              getSourceSelects(muxIn0);

            // in0 must be a high impedance value, so that when sel == 0
            // the tristate buffer is closed
            cout << "in0 sources" << endl;
            for (auto sel : muxIn0Srcs) {
              cout << "\t" << sel->toString() << endl;

              Wireable* src = extractSource(sel);
              assert(isa<Instance>(src));

              Instance* driver = cast<Instance>(src);
              cout << "opname = " << getQualifiedOpName(*driver) << endl;
              assert(getQualifiedOpName(*driver) == "rtlil.highImpedanceBit");
            }

            //Instance* muxPt = addPassthrough(srcMux, "_tristate_mux_PT");

            // Create tristate buffer, attache the rtMux select and rtmux drivers
            // to the tristate buffer

            cout << "Building tribuf" << endl;
            int muxWidth =
              srcMux->getModuleRef()->getGenArgs().at("WIDTH")->get<int>();
            Instance* tristate =
              def->addInstance(srcMux->toString() + "_tristate",
                               "coreir.tribuf",
                               {{"width", CoreIR::Const::make(def->getContext(), muxWidth)}});

            vector<Select*> selSrcs = getSourceSelects(muxSel);
            assert(selSrcs.size() == 1);

            def->connect(tristate->sel("en"), selSrcs[0]);

            vector<Select*> inVals = getSignalValues(muxIn1);
            assert(inVals.size() == (uint) muxWidth);

            for (uint i = 0; i < inVals.size(); i++) {
              def->connect(tristate->sel("in")->sel(i), inVals[i]);
            }

            // Note: Cannot wire up arrays of bitinout?

            for (uint i = 0; i < inVals.size(); i++) {
              def->connect(tristate->sel("out")->sel(i),
                           inst->sel("INOUT_DRIVER_PORT")->sel(i));
            }

            // Create passthrough around inst, then rewire

            cout << "Building triget" << endl;
            Instance* pt = addPassthrough(inst, "_padIO_PT");
            Instance* triget =
              def->addInstance(pt->toString() + "_triget",
                               "coreir.ibuf",
                               {{"width",
                                     CoreIR::Const::make(def->getContext(), muxWidth)}});

            for (int i = 0; i < muxWidth; i++) {
              def->connect(pt->sel("in")->sel("INOUT_PORT")->sel(i),
                           triget->sel("in")->sel(i));
            }

            for (int i = 0; i < muxWidth; i++) {
              def->connect(pt->sel("in")->sel("INOUT_PORT")->sel(i),
                           pt->sel("in")->sel("INOUT_DRIVER_PORT")->sel(i));
            }

            def->connect(triget->sel("out"), pt->sel("in")->sel("OUT_PORT"));

            cout << "Removing instance" << endl;

            def->removeInstance(inst);
            def->removeInstance(srcMux);
            inlineInstance(pt);

            def->validate();

            removedPad = true;
            break;

          }
        }
      }
    }
  }

}; //End CoreIRWriter struct

struct WriteCoreIRPass : public Yosys::Pass {
	WriteCoreIRPass() : Pass("write_coreir") { }

  virtual void execute(std::vector<std::string> args, RTLIL::Design *design) {

    cout << "String list" << endl;
    vector<string>::iterator argiter;
    string topname = "_top";
    for (argiter = args.begin(); argiter != args.end(); argiter++) {
      if (*argiter == "-top") {
        argiter++;
        if (argiter == args.end()) {
          cout << "Did not specify a top!" << endl;
        }
        topname = *argiter;
        cout << "setting top to " << topname << endl;
      }
    }

    Context* c = newContext();
    log_header(design, "Executing TOCOREIR pass (find stub nets).\n");

    // Find and create coreir stubs for all modules
    Namespace* g = c->getGlobal();

    CoreIRLoadLibrary_rtlil(c);

    
    CoreIRWriter corewriter(c,g,design);

    
    corewriter.buildModuleMap();

    cout << "---------- Built module map" << endl;

    // TODO: Pre-run to generate modules?
    cout << "Modules before  pre-run to generate parametric modules" << endl;
    for (auto& it : design->modules_) {
      string nm = id2cstr(it.first);

      cout << "\t" << id2cstr(it.first) << endl;
    }

    cout << "Modules after generating parametric modules" << endl;
    for (auto& it : design->modules_) {
      string nm = id2cstr(it.first);
      cout << "\t" << id2cstr(it.first) << endl;
    }
    
    // Now with all modules added create module definitions
    for (auto mpair : corewriter.modMap) {

      RTLIL::IdString rmodName = mpair.first;
      CoreIR::Module* mod = mpair.second;
      assert(mod);

      cout << "Processing module " << mod->getName() << endl;
      // cout << "Parameters for " << mod->getName() << endl;
      // for (auto& param : (it.second)->avail_parameters) {
      //   cout << "\t" << id2cstr(param) << endl;
      // }
      //SigMap assign_map(it.second);
      

      CoreIR::ModuleDef* def = mod->newModuleDef();

      RTLIL::Module* rmod = design->module(rmodName);

      map<Cell*, Instance*> instMap = corewriter.buildInstanceMap(rmod, def);

      cout << "# of instances in " << mod->getName() << " = " << instMap.size() << endl;

      corewriter.buildSelectMap(rmod, instMap, def);

      corewriter.removeRTLILTristate(def);
      cout << "Setting definition for module = " << mod->getName() << endl;
      mod->setDef(def);
    }

    cout << "Modules after running instance maps" << endl;
    for (auto& it : design->modules_) {
      string nm = id2cstr(it.first);

      cout << "\t" << id2cstr(it.first) << endl;
    }

    assert(corewriter.modMap.size() > 0);

    cout << "Modules after running instance maps" << endl;
    c->runPasses({"packbitconstants","packconnections"});
    
    string fileName = topname + ".json";
    cout << "Saving to " << fileName << endl;
    if (!saveToFile(g, fileName, nullptr)) {
      cout << "Could not save to json!!" << endl;
      c->die();
    }

    deleteContext(c);
  }
} ToCoreIRPass;

PRIVATE_NAMESPACE_END

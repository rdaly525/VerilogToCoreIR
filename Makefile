COREIR_HOME = /Users/

all: test


test: write_coreir.so
	#yosys -p "write_coreir -top top1" -m ./write_coreir.so smax1.v
	#yosys -p "write_coreir -top top2" -m ./write_coreir.so smax2.v
	yosys -p "write_coreir -top top3" -m ./write_coreir.so smax3.v

to_coreir.so: to_coreir.cc
	yosys-config --exec --cxx --cxxflags --ldflags -lcoreir -lcoreir-rtlil  -o $@ -shared $^ --ldlibs

write_coreir.so: write_coreir.cc
	yosys-config --exec --cxx --cxxflags --ldflags -lcoreir -lcoreir-rtlil  -o $@ -shared $^ --ldlibs

clean:
	rm -f test1.log test2.log test3.log
	rm -f write_coreir.so write_coreir.d

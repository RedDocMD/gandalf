read_verilog netlist/demo2.v cells/verilog/*.v
hierarchy -top adder_demo
proc
cd adder_demo
connect -set en 1'b1
connect -set rst_n 1'b1
cd ..
flatten
opt -purge -sat -full
show adder_demo
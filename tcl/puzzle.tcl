read_verilog netlist/puzzle.v cells/verilog/*.v
hierarchy -top puzzle
#check
proc
cd puzzle
connect -set enable 1'b1
connect -set rst_n 1'b1
cd ..
flatten
opt -purge -sat -all
show puzzle
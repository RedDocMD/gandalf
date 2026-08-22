read_verilog netlist/puzzle_fixed.v tb/extract_compare_outputs.v cells/verilog/*.v
prep -top checked_puzzle -flatten
select checked_puzzle
async2sync
sat -seq 140 -set-init-zero -set rst_n 1 -set enable 1 -set-at 140 matched 1 -dump_json /tmp/signals.json
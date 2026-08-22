import subprocess as sp
import tempfile
import json


def run_sat(filename, seqlen, dump, fix):
    dump_str = f'-dump_json {dump}'
    if fix:
        fixval, fixpos = fix
        fix_str = f'-set-at {fixpos} I {fixval}'
    else:
        fix_str = ''
    with tempfile.NamedTemporaryFile(suffix='.tcl', mode='w') as f:
        print(f"read_verilog {filename}", file=f)
        print("prep -top puzzle", file=f)
        print(f"sat -seq {seqlen} -set-init-zero -set-at {seqlen} success 1 {fix_str} {dump_str}", file=f)
        f.flush()
        return sp.check_output(f"yosys -s {f.name}", shell=True).decode()


def check_sat(filename, seqlen, dump, fix=None):
    output = run_sat(filename, seqlen, dump, fix)
    return 'no model found' not in output

def parse_waveform(wave):
    state = None
    bits = []
    for chr in wave:
        if chr == '1' or chr == '0':
            bits.append(chr)
            state = chr
        elif chr == '.':
            bits.append(state)
        else:
            bits.append('x')
            state = 'x'
    return bits


def get_input_bits(fname):
    with open(fname) as signal_file:
        waves = json.load(signal_file)
        signals = waves['signal']
        for signal in signals:
            if signal['name'] == 'I':
                bits = parse_waveform(signal['wave'])
                bits = bits[1:122]
                return bits


def make_bitstr(bits):
    return ''.join(bits)


filename = 'netlist/puzzle_flattened_success.v'
seq_len = 123
sig_len = 121

with tempfile.NamedTemporaryFile(suffix=".json", mode='w') as outfile:
    check_sat(filename, seq_len, outfile.name)
    solution_bits = get_input_bits(outfile.name)
    print(make_bitstr(solution_bits))
star_pos = [idx for idx, val in enumerate(solution_bits) if val == '1']
for pos in star_pos:
    with tempfile.NamedTemporaryFile(suffix=".json", mode='w') as outfile:
        found = check_sat(filename, seq_len, outfile.name, (0, pos + 1))
        print(f'Fixing {pos} to 0, {found}')
        if found:
            bits = get_input_bits(outfile.name)
            print(make_bitstr(bits))
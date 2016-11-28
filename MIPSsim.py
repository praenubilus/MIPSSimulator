from os import path
from mips32 import Instruction, Data
import argparse as ap

parser = ap.ArgumentParser(description='MIPS 32 Simulator by Yunhao Wan:1677-3116.')
parser.add_argument('inputfilename', help="path of input file, e.g. 'fibonacci.bin'.")
parser.add_argument('outputfilename', default='output.txt', help="path of output file, e.g. 'output'.")
parser.add_argument('operation', choices=['dis', 'sim'], help="Specify disassembly or simulation. The value can only "
                                                              "be either 'dis' or 'sim' without quotes.")
parser.add_argument('-T', dest='tracingoutput', help="Optional argument to specify the start (m) and end (n) cycles of "
                                                     "simulation tracing output. Tracing should be done in a "
                                                     "single-step fashion with the contents of registers and memory "
                                                     "shown after every processor cycle. -T0:0 indicates that no "
                                                     "tracing is to be performed; eliminating the argument specifies "
                                                     "that every cycle is to be traced. (will be implemented in Part "
                                                     "II)")
args = parser.parse_args()

if args.operation == 'sim':
    raise RuntimeError('The current project(Part 1) only supports disassembly. Simulation will be supported in '
                       'future part II')

# PC should be 32 bits
PC = 600
in_file_path = args.inputfilename
out_file_path = args.outputfilename
in_file_size = path.getsize(in_file_path)
inst_byte_size = 4

print("Binary file size is {} bytes. {} instructions/data total.".format(str(in_file_size), str(in_file_size / 4)))

read_buf = b''
is_EOF = False
is_break = False

with open(in_file_path, 'rb') as file_in:
    with open(out_file_path, 'wt') as file_out:

        while not is_EOF:
            read_buf = file_in.read(inst_byte_size)
            if len(read_buf) < inst_byte_size:
                is_EOF = True
                continue

            if is_break:
                write_buf = Data(data_binary=read_buf, pc_val=PC)
            else:
                write_buf = Instruction(instruction_binary=read_buf, pc_val=PC)
                is_break = write_buf.is_break()

            print(write_buf)
            file_out.write(str(write_buf) + '\n')

            PC += inst_byte_size

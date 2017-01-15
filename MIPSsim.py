from os import path
from mips32 import Instruction, Data
import argparse as ap
from pipeline import Pipeline
from toolkits import extract_binary_list

parser = ap.ArgumentParser(description='MIPS 32 Simulator by Yunhao Wan:1677-3116.')
parser.add_argument('inputfilename', help="path of input file, e.g. 'fibonacci.bin'.")
parser.add_argument('outputfilename', default='output.txt', help="path of output file, e.g. 'output'.")
parser.add_argument('operation', choices=['dis', 'sim'], help="Specify disassembly or simulation. The value can only "
                                                              "be either 'dis' or 'sim' without quotes.")
parser.add_argument('tracingoutput', help="Optional argument to specify the start (m) and end (n) cycles of "
                                          "simulation tracing output. Tracing should be done in a "
                                          "single-step fashion with the contents of registers and memory "
                                          "shown after every processor cycle. -T0:0 indicates that no "
                                          "tracing is to be performed; eliminating the argument specifies "
                                          "that every cycle is to be traced. (will be implemented in Part "
                                          "II)")
args = parser.parse_args()

operation = args.operation
# PC should be 32 bits
PC = 600
in_file_path = args.inputfilename
out_file_path = args.outputfilename
in_file_size = path.getsize(in_file_path)
inst_byte_size = 4
tracing_range = args.tracingoutput


def main():
    print("Binary file size is {} bytes. {} instructions/data total.".format(str(in_file_size), str(in_file_size / 4)))
    if operation == 'sim':
        simulator()
    elif operation == 'dis':
        dis_assembly()


def dis_assembly():
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


def simulator():
    inst_mem, data_mem, inst_bin_list, data_bin_list = extract_binary_list(in_file_path)
    ppl = Pipeline(inst_mem, data_mem)

    sub_range = tracing_range[3:-1]
    start, end = sub_range.split(sep=':')
    write_buf = None
    start = int(start)
    end = int(end)

    with open(out_file_path, 'wt') as file_out:
        cur_cycle = 0
        while not ppl.is_over:
            cur_cycle += 1
            ppl.next_cycle()
            if cur_cycle >= start and cur_cycle <= end:
                write_buf = 'Cycle <{}>:\n{}{}{}{}{}{}'.format(ppl.cycle, ppl.IQ, ppl.RS, ppl.ROB, ppl.BTB,
                                                               ppl.RF, ppl.DS)
                file_out.write(str(write_buf))


if __name__ == "__main__":
    main()

from os import path
from mips32 import Instruction, Data


def bitstring_to_bytes(s):
    v = int(s, 2)
    b = bytearray()
    while v:
        b.append(v & 0xff)
        v >>= 8
    return bytes(b[::-1])


def extract_binary_list(in_file_path='../test_files/fibonacci_bin.bin'):
    in_file_size = path.getsize(in_file_path)
    inst_byte_size = 4
    PC = 600

    print("Binary file size is {} bytes. {} instructions/data total.".format(str(in_file_size), str(in_file_size / 4)))

    read_buf = b''
    is_EOF = False
    is_break = False
    inst_mem = {}
    data_mem = {}
    inst_bin_list = []
    data_bin_list = []

    with open(in_file_path, 'rb') as file_in:

        while not is_EOF:
            read_buf = file_in.read(inst_byte_size)
            if len(read_buf) < inst_byte_size:
                # reach end of file
                break

            if not is_break:
                inst = Instruction(instruction_binary=read_buf, pc_val=PC)
                inst_mem[PC] = inst
                inst_bin_list.append(read_buf)
                is_break = inst.is_break()

            if is_break and PC >= 716:
                data = Data(data_binary=read_buf, pc_val=PC)
                data_mem[PC] = data
                data_bin_list.append(read_buf)

            PC += inst_byte_size

    return inst_mem, data_mem, inst_bin_list, data_bin_list

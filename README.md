# Computer Architecture: MIPS 32 Simulator

email: yunhao@ufl.edu

This project implements a MIPS32 simulator. Part I is a disassembler, Part II can simulator the mips instructions cycle by cycle.

## Part I
The program is written in Python 3. It has been tested under OSX 10.11.6 Python 3.5.2(the author's computer) and
Python 3.4.3(CISE thunder).

To run the program, compiling is not required. Just make sure both MIPSsim.py and mips32.py files are in the current folder/path. to run the program you may use the following format:
```python
python3 MIPSsim.py <inputfilename> <outputfilename> <operation> [-Tm:n]
```

For more information on how to use the arguments of the program, use the following command in the terminal/console:
```python
python3 MIPSsim.py -h
```

## Part II
This part implemented tomasulo Cycle by Cycle pipeline simulation. It can simulate the pipeline status for each cycle.

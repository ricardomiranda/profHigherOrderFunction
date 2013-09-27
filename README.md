This program compares the performance of Higher-order functions and First-order functions in Fortran. It also has OpenMP directives. To run it in Linux type on the command line:
 - ulimit -s unlimited
 - export OMP_NUM_THREADS=4
 - f95  -xopenmp=parallel -pg  -o HOF ModuleFL.f95  ModuleHOF.f95  MainHOF.f95
 - ./HOF
 - gprof HOF gmon.out>output.txt
 - cat output.txt 

---------

The code is under the GPL license so feel free to use it!

(c) Ricardo Miranda, 2013, mail@ricardomiranda.com.

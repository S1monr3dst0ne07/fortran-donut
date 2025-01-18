

all: build run


build:
	gfortran main.f90 -o main

run: main
	./main

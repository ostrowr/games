CC=g++

CFLAGS=-c -O3 -Wall -Wextra -pedantic-errors -Wuninitialized -Wmissing-declarations -std=c++14

all: solve

solve: board.o connect_4.o
	$(CC) board.o connect_4.o -o solve

board.o: board.cpp
	$(CC) $(CFLAGS) board.cpp

connect_4.o: connect_4.cpp
	$(CC) $(CFLAGS) connect_4.cpp

clean:
	rm *.o solve
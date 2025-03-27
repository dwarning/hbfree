# GNU make file 
# for hbw - harmonic balance WIN wrapper.
# Harmonic Balance Simulator wrapper for Win release
# wrapper version 0.1.0  dd. 06-09-2000
# G. Serdyuk, gserdyuk@mail.ru
# Public Domain

CC	=	g++ -g
LINK	=	g++ -g

PROG	=	hbw.exe

OBJ	=	\
		hbw.o

$(PROG): $(OBJ)
	$(LINK) -o $@ $<

hbw.o: hbw.cc err.h

%.o: %.cc
	$(CC) -c -o $@ $<
	
clean:
	rm *.o

rmtmp:
	rm nodes???????
	rm elems???????
	rm s2hout???????

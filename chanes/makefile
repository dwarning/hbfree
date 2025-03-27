
CC	=	gcc
F77	=	gfortran
CPPC	=	c++
LIBR	=	ar
RANLIB	=	ranlib
PROG	=	../hbl

FFLAGS = 	-fautomatic \
		-g \
		-malign-double \
		-O \
		-fdefault-real-8 \
		-fdefault-double-8 \
		-freal-4-real-8 \
		-std=legacy

#-Wall
#-fbounds-check
#-fdebug-kludge -fwritable-strings  


OBJ_DIR = obj


F_SRC =  \
	charint.f	\
	decode.f 	\
	detsyn.f 	\
	former.f 	\
	frequen.f 	\
	ftmas2.f 	\
	harm.f  	\
	initi.f 	\
	inkoor.f 	\
	koord.f  	\
	krdchk.f	\
	lena.f  	\
	len.f 		\
	lineq1.f 	\
	luback.f 	\
	lucan.f 	\
	lufrw.f 	\
	luslv.f 	\
	main.f  \
	otchm.f \
	packm.f \
	regul.f \
	restor.f	\
	sort.f  \
	sortuz.f	\
	stback.f	\
	stepfr.f	\
	sumdif.f	\
	suprim.f	\
	topo.f	\
	topoin.f	\
	zgradie.f	\
	ziny.f	\
	zlserch.f	\
	zmache.f	\
	znef.f	\
	zneinck.f	\
	znejac.f	\
	znewsol.f	\
	zstop0.f	\
	zstop.f 

LIB_DIR = ./libelem

LIB_SRC = \
	libelem/biptr.f \
	libelem/cbarr.f \
	libelem/cdiff.f \
	libelem/clin.f \
	libelem/cpoly.f \
	libelem/curt.f \
	libelem/cusd.f \
	libelem/discont.f \
	libelem/icujunc.f \
	libelem/icupoly.f \
	libelem/indsv.f \
	libelem/junc.f \
	libelem/lib0.f \
	libelem/liblin.f \
	libelem/libmod.f \
	libelem/lin.f \
	libelem/mdsch.f \
	libelem/mpl.f \
	libelem/poly5.f \
	libelem/shleif.f \
	libelem/stab.f \
	libelem/svlutl.f \
	libelem/test.f \
	libelem/ytab.f 

F_OBJ = \
	$(OBJ_DIR)/charint.o \
	$(OBJ_DIR)/decode.o \
	$(OBJ_DIR)/detsyn.o \
	$(OBJ_DIR)/former.o \
	$(OBJ_DIR)/frequen.o \
	$(OBJ_DIR)/ftmas2.o \
	$(OBJ_DIR)/harm.o \
	$(OBJ_DIR)/initi.o \
	$(OBJ_DIR)/inkoor.o \
	$(OBJ_DIR)/koord.o \
	$(OBJ_DIR)/krdchk.o \
	$(OBJ_DIR)/lena.o \
	$(OBJ_DIR)/len.o \
	$(OBJ_DIR)/lineq1.o \
	$(OBJ_DIR)/luback.o \
	$(OBJ_DIR)/lucan.o \
	$(OBJ_DIR)/lufrw.o \
	$(OBJ_DIR)/luslv.o \
	$(OBJ_DIR)/main.o \
	$(OBJ_DIR)/otchm.o \
	$(OBJ_DIR)/packm.o \
	$(OBJ_DIR)/regul.o \
	$(OBJ_DIR)/restor.o \
	$(OBJ_DIR)/sort.o \
	$(OBJ_DIR)/sortuz.o \
	$(OBJ_DIR)/stback.o \
	$(OBJ_DIR)/stepfr.o \
	$(OBJ_DIR)/sumdif.o \
	$(OBJ_DIR)/suprim.o \
	$(OBJ_DIR)/topo.o \
	$(OBJ_DIR)/topoin.o \
	$(OBJ_DIR)/zgradie.o \
	$(OBJ_DIR)/ziny.o \
	$(OBJ_DIR)/zlserch.o \
	$(OBJ_DIR)/zmache.o \
	$(OBJ_DIR)/znef.o \
	$(OBJ_DIR)/zneinck.o \
	$(OBJ_DIR)/znejac.o \
	$(OBJ_DIR)/znewsol.o \
	$(OBJ_DIR)/zstop0.o \
	$(OBJ_DIR)/zstop.o 
	
LIB_OBJ = \
	$(OBJ_DIR)/biptr.o \
	$(OBJ_DIR)/cbarr.o \
	$(OBJ_DIR)/cdiff.o \
	$(OBJ_DIR)/clin.o \
	$(OBJ_DIR)/cpoly.o \
	$(OBJ_DIR)/curt.o \
	$(OBJ_DIR)/cusd.o \
	$(OBJ_DIR)/discont.o \
	$(OBJ_DIR)/icujunc.o \
	$(OBJ_DIR)/icupoly.o \
	$(OBJ_DIR)/indsv.o \
	$(OBJ_DIR)/junc.o \
	$(OBJ_DIR)/lib0.o \
	$(OBJ_DIR)/liblin.o \
	$(OBJ_DIR)/libmod.o \
	$(OBJ_DIR)/lin.o \
	$(OBJ_DIR)/mdsch.o \
	$(OBJ_DIR)/mpl.o \
	$(OBJ_DIR)/poly5.o \
	$(OBJ_DIR)/shleif.o \
	$(OBJ_DIR)/stab.o \
	$(OBJ_DIR)/svlutl.o \
	$(OBJ_DIR)/test.o \
	$(OBJ_DIR)/ytab.o 

.all: $(PROG)

$(PROG): $(F_OBJ) $(LIB_OBJ)
		$(F77) -o $@ $^
		
$(F_OBJ): $(OBJ_DIR)/%.o: %.f	
		$(F77) -c $(FFLAGS) -o $@ $< 

$(LIB_OBJ): $(OBJ_DIR)/%.o: $(LIB_DIR)/%.f		
		$(F77) -c $(FFLAGS) -o $@ $< 

touch:
	touch $(F_SRC) $(LIB_SRC) $(QUP_SRC)
		
clean:
	rm -f $(F_OBJ) $(LIB_OBJ)

#includes:
$(OBJ_DIR)/former.o:  circuit.i charint.i
$(OBJ_DIR)/frequen.o: circuit.i charint.i
$(OBJ_DIR)/len.o:     circuit.i charint.i
$(OBJ_DIR)/main.o:    circuit.i
$(OBJ_DIR)/sortuz.o:  circuit.i
$(OBJ_DIR)/suprim.o:  circuit.i
$(OBJ_DIR)/topo.o:    circuit.i
$(OBJ_DIR)/topoin.o:  circuit.i
$(OBJ_DIR)/znef.o:    circuit.i funcsize.i      
$(OBJ_DIR)/znejac.o:  circuit.i funcsize.i
$(OBJ_DIR)/inkoor.o:  funcsize.i

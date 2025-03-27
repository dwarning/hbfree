
#
#   M A K E F I L E . G N U  --  S2H-compiler:  Making file
#
#   22.03.2000 - create by Alexandr S. Belogolovsky (ASB)
#                                      (a_b@i.am, spice3man@yahoo.com)
#
#   Usage:
#       MAKE        to making
#       MAKE -n     to debug
#
#   Note (GNU MAKE automatic variables): 
#    $@	The file name of the target of the rule. If the target is an archive member, then `$@' is the name of the archive file. In a pattern rule that has multiple targets (see section Introduction to Pattern Rules), `$@' is the name of whichever target caused the rule's commands to be run. 
#    $%	The target member name, when the target is an archive member. See section Using make to Update Archive Files. For example, if the target is `foo.a(bar.o)' then `$%' is `bar.o' and `$@' is `foo.a'. `$%' is empty when the target is not an archive member. 
#    $<	The name of the first dependency. If the target got its commands from an implicit rule, this will be the first dependency added by the implicit rule (see section Using Implicit Rules). 
#    $?	The names of all the dependencies that are newer than the target, with spaces between them. For dependencies which are archive members, only the member named is used (see section Using make to Update Archive Files). 
#    $^	The names of all the dependencies, with spaces between them. For dependencies which are archive members, only the member named is used (see section Using make to Update Archive Files). A target has only one dependency on each other file it depends on, no matter how many times each file is listed as a dependency. So if you list a dependency more than once for a target, the value of $^ contains just one copy of the name. 
#    $+	This is like `$^', but dependencies listed more than once are duplicated in the order they were listed in the makefile. This is primarily useful for use in linking commands where it is meaningful to repeat library file names in a particular order. 
#    $*	The stem with which an implicit rule matches. If the target is `dir/a.foo.b' and the target pattern is `a.%.b' then the stem is `dir/foo'. The stem is useful for constructing names of related files. In a static pattern rule, the stem is part of the file name that matched the `%' in the target pattern. In an explicit rule, there is no stem; so `$*' cannot be determined in that way. Instead, if the target name ends with a recognized suffix (see section Old-Fashioned Suffix Rules), `$*' is set to the target name minus the suffix. For example, if the target name is `foo.c', then `$*' is set to `foo', since `.c' is a suffix. GNU make does this bizarre thing only for compatibility with other implementations of make. You should generally avoid using `$*' except in implicit rules or static pattern rules. If the target name in an explicit rule does not end with a recognized suffix, `$*' is set to the empty string for that rule. 
#`$(@D)' The directory part of the file name of the target, with the trailing slash removed. If the value of `$@' is `dir/foo.o' then `$(@D)' is `dir'. This value is `.' if `$@' does not contain a slash. 
#`$(@F)' The file-within-directory part of the file name of the target. If the value of `$@' is `dir/foo.o' then `$(@F)' is `foo.o'. `$(@F)' is equivalent to `$(notdir $@)'. 
#`$(*D)' `$(*F)' The directory part and the file-within-directory part of the stem; `dir' and `foo' in this example. 
#`$(%D)' `$(%F)' The directory part and the file-within-directory part of the target archive member name. This makes sense only for archive member targets of the form `archive(member)' and is useful only when member may contain a directory name. (See section Archive Members as Targets.) 
#`$(<D)' `$(<F)' The directory part and the file-within-directory part of the first dependency. 
#`$(^D)' `$(^F)' Lists of the directory parts and the file-within-directory parts of all dependencies. 
#`$(?D)' `$(?F)' Lists of the directory parts and the file-within-directory parts of all dependencies that are newer than the target. 
#
#    F.e.:
# for rule  D:\Dpath\Dname.EXT:  S:\Spath\Sname.ext  etc ...
#	$<	=	S:\Spath\Dname.EXT
#	$*	=	D:\Dpath\Dname
#	$(*F)	=	\Dname
#	$(<F)	=	Sname.ext

SPROGNAME = ../../s2h
                                   # Compiler EXE-file name
LEXCNV	= cat
                                   # Convertor for provide compatibility LEXYY.C
                                   # with GNU C
CC	= gcc -g
                                   # C compiler name
LBR	= ar
                                   # (Object) files Librarien
RANLIB	= ranlib
                                   # 
LINK	= ld
                                   # Linker
#HD	= T:/i386-mingw32/include
                                   # GNU C headers directory
#LIB	= T:/i386-mingw32/lib
                                   # GNU C libraries directory

LEXDIR  = lexlib
				   # Lex-library sources

LEX	= flex -l 
#-d -T 

#YACC	= yacc
YACC	= bison -y 

# Options for C compiler [-I$(HD) -L$(LIB)]:
OPT	= -DGNU_C
                                   # - normal
#OPT	= -DGNU_C -g
                                   # - for debug

# Working/temporary directory:
WRKDIR  = obj

OBJ = \
	$(WRKDIR)/shiofun.o\
	$(WRKDIR)/shpars.o\
	$(WRKDIR)/shparsy.o\
	$(WRKDIR)/shlpars.o\
	$(WRKDIR)/shpass1.o\
	$(WRKDIR)/shpass2.o\
	$(WRKDIR)/shconfig.o
OBJF= shiofun.o shpars.o shparsy.o shlpars.o shpass1.o shpass2.o shconfig.o
                                   # Modules for object library

#!#$(WRKDIR)/$(sPROGNAME).EXE:  shmain.c $(OBJ)
#!#	$(CC) $(OPT) -o $@ $< $(OBJ) -llex

# if generate liblex.lib here:
$(WRKDIR)/$(SPROGNAME):  shmain.c $(WRKDIR)/libshpr.a $(WRKDIR)/liblex.a
	$(CC) $(OPT) -o $@ $< -L$(WRKDIR) -lshpr -llex -lm

$(WRKDIR)/libshpr.a :  $(OBJ)
	$(LBR)	rc $@ $(OBJ)
	$(RANLIB) $@

$(WRKDIR)/shpars.c y.tab.h:    shpars.y
	$(YACC)	-r all -d -y $<
	mv  y.tab.c $(WRKDIR)/shpars.c


$(WRKDIR)/shpars.o :  $(WRKDIR)/shpars.c shcommon.h	
	$(CC)   -c $(OPT) -I . -o $@  $<
$(WRKDIR)/shparsy.o : shparsy.c shcommon.h y.tab.h	
	$(CC)   -c $(OPT) -o $@ $<

$(WRKDIR)/shlpars.c : shlpars.l
	$(LEX)  -o$(WRKDIR)/shlpars.c $<

# $(LEXCNV) <lex.yy.c >$(WRKDIR)/shlpars.c
#	rm lex.yy.c

$(WRKDIR)/shlpars.o : $(WRKDIR)/shlpars.c y.tab.h	
	$(CC)   -c $(OPT) -I . -o $@ $<

$(WRKDIR)/shconfig.o : shconfig.c shcommon.h y.tab.h	
	$(CC)   -c $(OPT) -o $@ $<
$(WRKDIR)/shiofun.o : shiofun.c shcommon.h y.tab.h	
	$(CC)   -c $(OPT) -o $@ $<
$(WRKDIR)/shpass1.o : shpass1.c shcommon.h y.tab.h	
	$(CC)   -c $(OPT) -o $@ $<
$(WRKDIR)/shpass2.o : shpass2.c shcommon.h y.tab.h	
	$(CC)   -c $(OPT) -o $@ $<

shcommon.h :  shmsgen.h shmain.h shiofun.h shlpars.h shpars.h shconfig.h shexcode.h
# no actions

#----- Lex-library ------
$(WRKDIR)/liblex.a:	$(WRKDIR)/allprint.o $(WRKDIR)/reject.o $(WRKDIR)/yyless.o $(WRKDIR)/yywrap.o
#	$(LBR)	q liblex.a allprint.o reject.o yyless.o yywrap.o
	$(LBR)	rc liblex.a $?
	$(RANLIB) liblex.a
	mv liblex.a $(WRKDIR)/liblex.a
$(WRKDIR)/allprint.o:	$(LEXDIR)/allprint.c
	$(CC)   -c $(OPT) -o $@ $<
$(WRKDIR)/reject.o:	$(LEXDIR)/reject.c
	$(CC)   -c $(OPT) -o $@ $<
$(WRKDIR)/yyless.o:	$(LEXDIR)/yyless.c
	$(CC)   -c $(OPT) -o $@ $<
$(WRKDIR)/yywrap.o:	$(LEXDIR)/yywrap.c
	$(CC)   -c $(OPT) -o $@ $<


clean:
	rm $(WRKDIR)/*
	
work:
	cp $(WRKDIR)/$(SPROGNAME) ../$(SPROGNAME)
	cp $(WRKDIR)/$(SPROGNAME) ../examples/$(SPROGNAME)
	
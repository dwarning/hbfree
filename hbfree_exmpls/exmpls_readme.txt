here are test examples.

di1.ckt		1-doide circuit with "t" transmission line under 1-tone excitation
di2.ckt		1-doide circuit with "t" transmission line under 2-tone excitation
di3.ckt		1-doide circuit without transmission line under 2-tone excitation
di4.ckt		as di2.ckt and with extremely large LO signal (15 Volt)
di5.ckt		as di4.ckt, but have a sintax error in input file
di6.ckt   	4-diode bridge rectifier under 2-tone excitation
di7.ckt 	4-diode bridge rectifier under 1-tone excitation
di8.ckt 	too large circuit

test		testing script
./results 	contains reference results


CURRENT TEST STATUS (April, 2004) :
-----------------------------------

TEST        binary release2000          HbFree              
di1.ckt		ok                          OK
di2.ckt		ok                          no convergence (small step)
di3.ckt		ok                          no convergence (small step)
di4.ckt		ok                          no convergence (small step)
di5.ckt		-
di6.ckt   	ok                          no convergence (local failure)
di7.ckt 	ok                          OK
di8.ckt 	-                           converged

NB. That means some problems still persist in open code - 
most probably - boundary violations (remember - fortran with static arrays)
Gennady Serdyuk

    

# It's simple...

autoboot.bin: PiSpigot/PiSpigot.cim
	cp PiSpigot/PiSpigot.cim autoboot.bin


PiSpigot/PiSpigot.cim: PiSpigot.asm
	zmac/zmac PiSpigot.asm --od PiSpigot --oo cim,lst -c -s -g


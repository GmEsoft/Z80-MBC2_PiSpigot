zmac\zmac PiSpigot.asm --od PiSpigot --oo cim,lst -c -s -g
if errorlevel 1 goto :eof
copy PiSpigot\PiSpigot.cim autoboot.bin

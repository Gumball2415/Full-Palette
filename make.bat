ca65 -g -o full_palette_persune.o full_palette.s -l list.txt
ld65 -v -t nes --dbgfile full_palette_persune.dbg full_palette_persune.o -o full_palette_persune.nes
full_palette_persune.nes

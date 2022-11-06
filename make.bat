mkdir output
ca65 -g -o full_palette_persune.o full_palette.s -l output/list.txt
ld65 -v -C nrom128.cfg --dbgfile output/full_palette_persune.dbg full_palette_persune.o -o output/full_palette_persune.nes

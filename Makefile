UC = $(shell echo '$1' | tr '[:lower:]' '[:upper:]')

PROJECT	:= melodius
AS		:= ca65
LD		:= ld65
MKDIR	:= mkdir -p
RMDIR	:= rmdir -p
CONFIG  := ./$(PROJECT).cfg
ASFLAGS	:= --cpu 65C02 -g
LDFLAGS	:= -C $(CONFIG)
SRC		:= ./src
OBJ		:= ./obj
SRCS	:= $(wildcard $(SRC)/*.s)
OBJS    := $(patsubst $(SRC)/%.s,$(OBJ)/%.o,$(SRCS))
EXE		:= $(call UC,$(PROJECT).PRG)
SDCARD	:= ./sdcard.img
MAPFILE := ./$(PROJECT).map
SYMFILE := ./$(PROJECT).sym

default: all

all: $(EXE)

$(EXE): releasedate $(OBJS) $(CONFIG)
	$(LD) $(LDFLAGS) $(OBJS) -m $(MAPFILE) -Ln $(SYMFILE) -o $@ 

$(OBJ)/%.o: $(SRC)/%.s | $(OBJ)
	$(AS) $(ASFLAGS) $< -o $@

$(OBJ):
	$(MKDIR) $@

$(SDCARD): $(EXE)
	$(RM) $(SDCARD)
	truncate -s 100M $(SDCARD)
	parted -s $(SDCARD) mklabel msdos mkpart primary fat32 2048s -- -1
	mformat -i $(SDCARD)@@1M -v $(call UC,$(PROJECT)) -F
	cp $(EXE) ROOT/
	mcopy -i $(SDCARD)@@1M -o -s -v -m ROOT/* ::

.PHONY: clean run releasedate
clean:
	$(RM) $(EXE) $(OBJS) $(SDCARD) $(MAPFILE) $(SYMFILE)

releasedate:
	/bin/echo -n $$(/bin/date '+%Y%m%d') > src/releasedate.inc

box: $(EXE) $(SDCARD)
	box16 -sdcard $(SDCARD) -prg $(EXE) -run -ram 1024

run: $(EXE) $(SDCARD)
	x16emu -sdcard $(SDCARD) -prg $(EXE) -debug -scale 2 -run -ram 1024
#	x16emu -sdcard $(SDCARD) -prg $(EXE) -debug -scale 2 -run
	

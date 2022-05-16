# Options
CC			= gcc
CCFLAGS	= -std=c11 -O3 -Wall -Wextra -pedantic 

FC			= gfortran
FCFLAGS	= -std=f2008 -O3 -Wall


F_MRECIP_FLAGS = -ffree-form -ffast-math -mrecip

LINKER	= $(CC)
LFLAGS	= -g -lglfw -lvulkan -ldl -lm

DEBUGGER = gdb

# Directories
TARGET	= bitmask_renderer
SRC_DIR	= src
BIN_DIR	= bin
OBJ_DIR	= .obj
MOD_DIR	= .mod
SPV_DIR  = spv

GC       = ./glsl.sh

C_SRC = $(wildcard $(SRC_DIR)/*.c $(SRC_DIR)/*/*.c)		
F_SRC = $(wildcard $(SRC_DIR)/*.f03 $(SRC_DIR)/*/*.f03)
GLSL  = $(wildcard $(SRC_DIR)/*.*.glsl $(SRC_DIR/*/*.*.glsl))
C_OBJ	= $(C_SRC:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
F_OBJ	= $(F_SRC:$(SRC_DIR)/%.f03=$(OBJ_DIR)/%.o)
MOD	= $(wildcard $(MOD_DIR)/*.mod)
SPV   = $(wildcard $(SPV_DIR)/*.spv)

build: $(BIN_DIR) $(BIN_DIR)/$(TARGET)

$(BIN_DIR): 
	@mkdir -p $@

$(C_OBJ): $(OBJ_DIR)/%.o : $(SRC_DIR)/%.c 
	@mkdir -p $(@D)
	$(CC) -c $(CCFLAGS) $< -o $@

$(MOD_DIR):
	@mkdir -p $@

$(F_OBJ): $(OBJ_DIR)/%.o : $(SRC_DIR)/%.f03
	@mkdir -p $(@D)
	$(FC) -c $(FCFLAGS) $< -o $@ -J $(MOD_DIR)

$(SPV_DIR):
	@mkdir -p $@

$(BIN_DIR)/$(TARGET): $(C_OBJ) $(F_OBJ)
	$(LINKER) $(C_OBJ) $(F_OBJ) $(LFLAGS) -o $(BIN_DIR)/$(TARGET)
	$(GC)

run: build
	./$(BIN_DIR)/$(TARGET)

debug: build
	$(DEBUGGER) $(BIN_DIR)/$(TARGET)

clean:
	rm -f $(C_OBJ)
	rm -f $(F_OBJ)
	rm -f $(MOD)
	rm -f $(SPV)

remove: clean
	rm -f $(BIN_DIR)/$(TARGET)


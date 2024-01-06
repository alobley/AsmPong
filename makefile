ASM=nasm

SRC_DIR=src
BUILD_DIR=build

FILE=game


#
# Floppy Image
#
floppy_image: $(BUILD_DIR)/main.img
$(BUILD_DIR)/main.img: file
	dd if=/dev/zero of=$(BUILD_DIR)/main.img bs=512 count=2880
	mkfs.fat -F 12 -n "MYOS" $(BUILD_DIR)/main.img
	dd if=$(BUILD_DIR)/$(FILE).bin of=$(BUILD_DIR)/main.img conv=notrunc

#
# File
#
file: $(BUILD_DIR)/$(FILE).bin
$(BUILD_DIR)/$(FILE).bin:
	$(ASM) $(SRC_DIR)/$(FILE).asm -f bin -o $(BUILD_DIR)/$(FILE).bin
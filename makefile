boot:
	nasm -f bin boot.asm -o bin/boot.bin
kernel:
	nasm -f bin kernel.asm -o bin/KERNEL.BIN

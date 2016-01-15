fdboot:
	nasm -UHD -f bin boot.asm -o bin/fdboot.bin
hdboot:
	nasm -DHD -f bin boot.asm -o bin/hdboot.bin
kernel:
	nasm -f bin kernel.asm -o bin/KERNEL.BIN

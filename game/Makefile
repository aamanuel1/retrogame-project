all: assemble run

assemble: jailbreak.s
	dasm jailbreak.s -ojailbreak.prg -lout.lst

run: jailbreak.prg
	xvic jailbreak.prg

clean:
	rm -f jailbreak.prg out.lst

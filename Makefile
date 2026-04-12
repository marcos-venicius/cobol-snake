main: main.cbl
	cobc -x -o main main.cbl

clean:
	rm main

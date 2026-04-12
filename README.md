# Snake Game in Cobol

This is meant to run on a linux.

I'm using **GnuCOBOL 4.0-early-dev.0**.

The code has a specific place with a specific linux instruction (clear command).

## Movements

- `k` Top
- `l` Right
- `j` Bottom
- `h` Right

## How to play?

You need to compile the program with `make`.
Then, you need to run the program `./main`.
Now, you open the file `./key-input.txt` in a normal text editor.
After doing this, you can just write one of the [directions above](#movements) and save the file **in time**.

## Info

There is no thread sleep here. Just a infinite loop and I render the game and apply game logics every `15000000` runs.

I know, it's terrible.

I didn't wanted to use external implementations or call C bindings for anything.

I just a call to `system` once, `CALL "SYSTEM" USING "clear"`.


Here is an example of the game running:

./examples/example.mp4



**Theoretically you can keep running the game until the snake is the size of the board; never tried**


**I'm iterating for every snake cell on every single pixel to check if there is a cell in that position, that's horrible**

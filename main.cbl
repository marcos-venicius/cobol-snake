       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLAYER-GAME.
       AUTHOR. MARCOS-VENICIUS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KEY-INPUT-FILE ASSIGN TO "key-input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD KEY-INPUT-FILE.
       01 MOVEMENT-REC.
         05 MOVEMENT-KEY PIC X(1).

       WORKING-STORAGE SECTION.
       01 GRID-WIDTH             PIC 9(2)  VALUE 10.
       01 GRID-HEIGHT            PIC 9(2)  VALUE 10.
       01 PLAYER-DIR-X           PIC S9(1) VALUE 1.
       01 PLAYER-DIR-Y           PIC S9(1) VALUE 0.
       01 NEXT-PLAYER-X          PIC S9(1) VALUE 0.
       01 NEXT-PLAYER-Y          PIC S9(1) VALUE 0.
       01 FRUIT-X                PIC S9(2) VALUE 5.
       01 FRUIT-Y                PIC S9(2) VALUE 5.
       01 L-ROW                  PIC 9(2)  VALUE 0.
       01 L-COL                  PIC 9(2)  VALUE 0.
       01 POINTS                 PIC 9(3)  VALUE 0.
       01 RENDER-CONTROL         PIC 9(8)  VALUE 0.
       01 FRAMES-BETWEEN-RENDER  PIC 9(8)  VALUE 15000000.
       01 SNAKE-SIZE             PIC 9(3)  VALUE 1.
       01 SNAKE.
         05 SNAKE-CELL PIC 9(4) OCCURS 100 TIMES
           INDEXED BY SNAKE-IDX.
       01 PLAYER-X               PIC 9(2)  VALUE 0.
       01 PLAYER-Y               PIC 9(2)  VALUE 0.
       01 TAIL-X                 PIC 9(2)  VALUE 0.
       01 TAIL-Y                 PIC 9(2)  VALUE 0.
       01 RENDERED               PIC 9(1)  VALUE 0.

       PROCEDURE DIVISION.
       0001-MAIN-LOGIC.
           MOVE 0 TO SNAKE-CELL(1)

           PERFORM UNTIL 1 < 0
             SET SNAKE-IDX TO 1

             IF RENDER-CONTROL = 0
      * Linux specific
               CALL "SYSTEM" USING "clear"

               IF SNAKE-SIZE >= 3
                 DISPLAY "You won everything with " POINTS " points!!!"
                 MOVE 0 TO RETURN-CODE
                 STOP RUN
               END-IF

               PERFORM 0001-MOVEMENT-INPUT-LOGIC
               PERFORM 0001-AUTO-MOVEMENT-LOGIC
               PERFORM 0001-UPDATE-SNAKE-POSITIONS-LOGIC
               PERFORM 0001-GAME-LOOP
             END-IF

             ADD 1 TO RENDER-CONTROL

             IF RENDER-CONTROL = FRAMES-BETWEEN-RENDER
               MOVE 0 TO RENDER-CONTROL
             END-IF
           END-PERFORM.

           STOP RUN.

       0001-GAME-LOOP.
           PERFORM VARYING L-ROW FROM 0 BY 1 UNTIL L-ROW >= GRID-WIDTH
             PERFORM VARYING L-COL
               FROM 0 BY 1 UNTIL L-COL >= GRID-HEIGHT

               MOVE 0 TO RENDERED

               DISPLAY SPACES WITH NO ADVANCING

               PERFORM VARYING SNAKE-IDX FROM 1 BY 1 
                 UNTIL SNAKE-IDX > SNAKE-SIZE

                 COMPUTE TAIL-X = SNAKE-CELL(SNAKE-IDX) / 16
                 COMPUTE TAIL-Y = FUNCTION 
                   MOD(SNAKE-CELL(SNAKE-IDX), 16)

                 IF TAIL-X = L-COL AND TAIL-Y = L-ROW
                   IF SNAKE-IDX = 1
                     DISPLAY "O" WITH NO ADVANCING
                   ELSE
                     DISPLAY "o" WITH NO ADVANCING
                   END-IF
                   MOVE 1 TO RENDERED
                   EXIT PERFORM
                 END-IF
               END-PERFORM



               IF RENDERED = 0
                 IF FRUIT-X <> -1 OR FRUIT-Y <> -1
                   IF L-ROW = FRUIT-Y AND L-COL = FRUIT-X
                     DISPLAY "+" WITH NO ADVANCING
                   ELSE
                     DISPLAY "." WITH NO ADVANCING
                   END-IF
                 ELSE
                   DISPLAY "." WITH NO ADVANCING
                 END-IF
               END-IF

               IF PLAYER-X = FRUIT-X AND PLAYER-Y = FRUIT-Y
                 ADD 1 TO POINTS
                 PERFORM 0001-SPAWN-FRUIT
                 PERFORM 0001-INCREASE-SNAKE-BODY
               END-IF
             END-PERFORM
             DISPLAY SPACES
           END-PERFORM.

           DISPLAY " ".
           DISPLAY "POINTS: " POINTS.

       0001-MOVEMENT-INPUT-LOGIC.
           OPEN INPUT KEY-INPUT-FILE.

           READ KEY-INPUT-FILE
           END-READ.
           CLOSE KEY-INPUT-FILE.

           IF MOVEMENT-KEY = 'k'
             MOVE -1 TO PLAYER-DIR-Y
             MOVE 0  TO PLAYER-DIR-X
           END-IF.

           IF MOVEMENT-KEY = 'l'
             MOVE 0 TO PLAYER-DIR-Y
             MOVE 1 TO PLAYER-DIR-X
           END-IF.

           IF MOVEMENT-KEY = 'j'
             MOVE 1 TO PLAYER-DIR-Y
             MOVE 0 TO PLAYER-DIR-X
           END-IF.

           IF MOVEMENT-KEY = 'h'
             MOVE 0  TO PLAYER-DIR-Y
             MOVE -1 TO PLAYER-DIR-X
           END-IF.

       0001-AUTO-MOVEMENT-LOGIC.
           COMPUTE NEXT-PLAYER-X = PLAYER-X + PLAYER-DIR-X.
           COMPUTE NEXT-PLAYER-Y = PLAYER-Y + PLAYER-DIR-Y.

           IF NEXT-PLAYER-X >= GRID-WIDTH
             MOVE 0 TO NEXT-PLAYER-X
           END-IF.

           IF NEXT-PLAYER-X < 0
             COMPUTE NEXT-PLAYER-X = GRID-WIDTH - 1
           END-IF.

           IF NEXT-PLAYER-Y >= GRID-HEIGHT
             MOVE 0 TO NEXT-PLAYER-Y
           END-IF.

           IF NEXT-PLAYER-Y < 0
             COMPUTE NEXT-PLAYER-Y = GRID-HEIGHT - 1
           END-IF.

           MOVE NEXT-PLAYER-X TO PLAYER-X.
           MOVE NEXT-PLAYER-Y TO PLAYER-Y.

       0001-UPDATE-SNAKE-POSITIONS-LOGIC.
           PERFORM VARYING SNAKE-IDX FROM SNAKE-SIZE BY -1 
             UNTIL SNAKE-IDX = 1

             MOVE SNAKE-CELL(SNAKE-IDX - 1) TO SNAKE-CELL(SNAKE-IDX)

           END-PERFORM.

           COMPUTE SNAKE-CELL(1) = (PLAYER-X * 16) + PLAYER-Y.

       0001-INCREASE-SNAKE-BODY.
           COMPUTE TAIL-X = SNAKE-CELL(SNAKE-SIZE) / 16.
           COMPUTE TAIL-Y = FUNCTION MOD(SNAKE-CELL(SNAKE-SIZE), 16)

           ADD PLAYER-DIR-X TO TAIL-X.
           ADD PLAYER-DIR-Y TO TAIL-Y.

           ADD 1 TO SNAKE-SIZE.

           COMPUTE SNAKE-CELL(SNAKE-SIZE) = (TAIL-X * 16) + TAIL-Y.

       0001-SPAWN-FRUIT.
           COMPUTE FRUIT-X = 
           FUNCTION INTEGER 
           ( (GRID-WIDTH - 1) * FUNCTION RANDOM ) + 1.

           COMPUTE FRUIT-Y = 
           FUNCTION INTEGER 
           ( (GRID-HEIGHT - 1) * FUNCTION RANDOM ) + 1.


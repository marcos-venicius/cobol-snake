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
       01 WS-GRID-WIDTH             PIC 9(2)  VALUE 10.
       01 WS-GRID-HEIGHT            PIC 9(2)  VALUE 10.
       01 WS-PLAYER-DIR-X           PIC S9(1) VALUE 1.
       01 WS-PLAYER-DIR-Y           PIC S9(1) VALUE 0.
       01 WS-NEXT-WS-PLAYER-X       PIC S9(1) VALUE 0.
       01 WS-NEXT-WS-PLAYER-Y       PIC S9(1) VALUE 0.
       01 WS-FRUIT-X                PIC S9(2) VALUE 5.
       01 WS-FRUIT-Y                PIC S9(2) VALUE 5.
       01 WS-FRUIT-LIFETIME         PIC S9(3) VALUE 0.
       01 WS-L-ROW                  PIC 9(2)  VALUE 0.
       01 WS-L-COL                  PIC 9(2)  VALUE 0.
       01 WS-POINTS                 PIC 9(3)  VALUE 0.
       01 WS-RENDER-CONTROL         PIC 9(8)  VALUE 0.
       01 WS-FRAMES-BETWEEN-RENDER  PIC 9(8)  VALUE 15000000.
       01 WS-SNAKE-SIZE             PIC 9(3)  VALUE 1.
       01 WS-SNAKE.
         05 SNAKE-CELL PIC 9(4) OCCURS 100 TIMES
           INDEXED BY SNAKE-IDX.
       01 WS-PLAYER-X               PIC 9(2)  VALUE 0.
       01 WS-PLAYER-Y               PIC 9(2)  VALUE 0.
       01 WS-TAIL-X                 PIC 9(2)  VALUE 0.
       01 WS-TAIL-Y                 PIC 9(2)  VALUE 0.
       01 WS-RENDERED               PIC 9(1)  VALUE 0.
       01 WS-START-TIMESTAMP        PIC 9(14).
       01 WS-START-DATETIME         PIC X(21).

       PROCEDURE DIVISION.
       0001-MAIN-LOGIC.
           PERFORM 0001-SPAWN-FRUIT.

           PERFORM 0001-CURRENT-TIME.

           COMPUTE WS-PLAYER-X =
           FUNCTION INTEGER
           ((WS-GRID-WIDTH - 1) *
           FUNCTION RANDOM(WS-START-TIMESTAMP)) + 1.

           COMPUTE WS-PLAYER-Y =
           FUNCTION INTEGER
           ( (WS-GRID-HEIGHT - 1) *
           FUNCTION RANDOM(WS-START-TIMESTAMP) ) + 1.

           COMPUTE SNAKE-CELL(1) = (WS-PLAYER-X * 16) + WS-PLAYER-Y.

           PERFORM UNTIL 1 < 0
             IF WS-RENDER-CONTROL = 0
      * Linux specific
               CALL "SYSTEM" USING "clear"

               IF WS-SNAKE-SIZE >= (WS-GRID-WIDTH * WS-GRID-HEIGHT)
                 DISPLAY "You won everything with " WS-POINTS
                                                  " points!!!"
                 MOVE 0 TO RETURN-CODE
                 STOP RUN
               END-IF

               PERFORM 0001-MOVEMENT-INPUT-LOGIC
               PERFORM 0001-AUTO-MOVEMENT-LOGIC
               PERFORM 0001-UPDATE-SNAKE-POSITIONS-LOGIC
               PERFORM 0001-GAME-LOOP
             END-IF

             IF WS-RENDER-CONTROL = WS-FRAMES-BETWEEN-RENDER
               MOVE 0 TO WS-RENDER-CONTROL
             ELSE
               ADD 1 TO WS-RENDER-CONTROL
             END-IF
           END-PERFORM.

           STOP RUN.

       0001-GAME-LOOP.
           PERFORM VARYING WS-L-ROW FROM 0 BY 1
             UNTIL WS-L-ROW >= WS-GRID-WIDTH
             PERFORM VARYING WS-L-COL
               FROM 0 BY 1 UNTIL WS-L-COL >= WS-GRID-HEIGHT

               MOVE 0 TO WS-RENDERED

               DISPLAY SPACES WITH NO ADVANCING

               PERFORM VARYING SNAKE-IDX FROM 1 BY 1
                 UNTIL SNAKE-IDX > WS-SNAKE-SIZE

                 COMPUTE WS-TAIL-X = SNAKE-CELL(SNAKE-IDX) / 16
                 COMPUTE WS-TAIL-Y = FUNCTION
                   MOD(SNAKE-CELL(SNAKE-IDX), 16)

                 IF WS-TAIL-X = WS-L-COL AND WS-TAIL-Y = WS-L-ROW
                   IF SNAKE-IDX = 1
                     DISPLAY "O" WITH NO ADVANCING
                   ELSE
                     DISPLAY "o" WITH NO ADVANCING
                   END-IF
                   MOVE 1 TO WS-RENDERED
                   EXIT PERFORM
                 END-IF
               END-PERFORM

               IF WS-RENDERED = 0
                 IF WS-FRUIT-X <> -1 OR WS-FRUIT-Y <> -1
                   IF WS-L-ROW = WS-FRUIT-Y AND WS-L-COL = WS-FRUIT-X
                     DISPLAY "+" WITH NO ADVANCING
                   ELSE
                     DISPLAY "." WITH NO ADVANCING
                   END-IF
                 ELSE
                   DISPLAY "." WITH NO ADVANCING
                 END-IF
               END-IF

               IF WS-PLAYER-X = WS-FRUIT-X AND WS-PLAYER-Y = WS-FRUIT-Y
                 ADD 1 TO WS-POINTS
                 PERFORM 0001-SPAWN-FRUIT
                 PERFORM 0001-INCREASE-SNAKE-BODY
               END-IF
             END-PERFORM
             DISPLAY SPACES
           END-PERFORM.

           IF WS-FRUIT-LIFETIME <= 0
             PERFORM 0001-SPAWN-FRUIT
           END-IF.

           ADD -1 TO WS-FRUIT-LIFETIME.

           DISPLAY " ".
           DISPLAY "WS-POINTS: " WS-POINTS.

       0001-MOVEMENT-INPUT-LOGIC.
           OPEN INPUT KEY-INPUT-FILE.

           READ KEY-INPUT-FILE
           END-READ.
           CLOSE KEY-INPUT-FILE.

           IF MOVEMENT-KEY = 'k'
             MOVE -1 TO WS-PLAYER-DIR-Y
             MOVE 0  TO WS-PLAYER-DIR-X
           END-IF.

           IF MOVEMENT-KEY = 'l'
             MOVE 0 TO WS-PLAYER-DIR-Y
             MOVE 1 TO WS-PLAYER-DIR-X
           END-IF.

           IF MOVEMENT-KEY = 'j'
             MOVE 1 TO WS-PLAYER-DIR-Y
             MOVE 0 TO WS-PLAYER-DIR-X
           END-IF.

           IF MOVEMENT-KEY = 'h'
             MOVE 0  TO WS-PLAYER-DIR-Y
             MOVE -1 TO WS-PLAYER-DIR-X
           END-IF.

       0001-AUTO-MOVEMENT-LOGIC.
           COMPUTE WS-NEXT-WS-PLAYER-X = WS-PLAYER-X + WS-PLAYER-DIR-X.
           COMPUTE WS-NEXT-WS-PLAYER-Y = WS-PLAYER-Y + WS-PLAYER-DIR-Y.

           IF WS-NEXT-WS-PLAYER-X >= WS-GRID-WIDTH
             MOVE 0 TO WS-NEXT-WS-PLAYER-X
           END-IF.

           IF WS-NEXT-WS-PLAYER-X < 0
             COMPUTE WS-NEXT-WS-PLAYER-X = WS-GRID-WIDTH - 1
           END-IF.

           IF WS-NEXT-WS-PLAYER-Y >= WS-GRID-HEIGHT
             MOVE 0 TO WS-NEXT-WS-PLAYER-Y
           END-IF.

           IF WS-NEXT-WS-PLAYER-Y < 0
             COMPUTE WS-NEXT-WS-PLAYER-Y = WS-GRID-HEIGHT - 1
           END-IF.

           MOVE WS-NEXT-WS-PLAYER-X TO WS-PLAYER-X.
           MOVE WS-NEXT-WS-PLAYER-Y TO WS-PLAYER-Y.

       0001-UPDATE-SNAKE-POSITIONS-LOGIC.
           IF WS-SNAKE-SIZE > 1
             PERFORM VARYING SNAKE-IDX FROM WS-SNAKE-SIZE BY -1
               UNTIL SNAKE-IDX <= 1

               MOVE SNAKE-CELL(SNAKE-IDX - 1) TO SNAKE-CELL(SNAKE-IDX)
             END-PERFORM
           END-IF.

           COMPUTE SNAKE-CELL(1) = (WS-PLAYER-X * 16) + WS-PLAYER-Y.

       0001-INCREASE-SNAKE-BODY.
           COMPUTE WS-TAIL-X = SNAKE-CELL(WS-SNAKE-SIZE) / 16.
           COMPUTE WS-TAIL-Y =
             FUNCTION MOD(SNAKE-CELL(WS-SNAKE-SIZE), 16)

           ADD WS-PLAYER-DIR-X TO WS-TAIL-X.
           ADD WS-PLAYER-DIR-Y TO WS-TAIL-Y.

           ADD 1 TO WS-SNAKE-SIZE.

           COMPUTE SNAKE-CELL(WS-SNAKE-SIZE) =
             (WS-TAIL-X * 16) + WS-TAIL-Y.

       0001-SPAWN-FRUIT.
           PERFORM 0001-CURRENT-TIME.

           COMPUTE WS-FRUIT-X =
           FUNCTION INTEGER
           ((WS-GRID-WIDTH - 1) *
             FUNCTION RANDOM(WS-START-TIMESTAMP)) + 1.

           COMPUTE WS-FRUIT-Y =
           FUNCTION INTEGER
           ((WS-GRID-HEIGHT - 1) *
             FUNCTION RANDOM(WS-START-TIMESTAMP)) + 1.

           COMPUTE WS-PLAYER-X = SNAKE-CELL(1) / 16.
           COMPUTE WS-PLAYER-Y = FUNCTION MOD(SNAKE-CELL(1), 16).
           COMPUTE WS-FRUIT-LIFETIME =
             WS-GRID-WIDTH * WS-GRID-HEIGHT * 0.5.

       0001-CURRENT-TIME.
           MOVE FUNCTION CURRENT-DATE TO WS-START-DATETIME.
           MOVE WS-START-DATETIME(1:14) TO WS-START-TIMESTAMP.

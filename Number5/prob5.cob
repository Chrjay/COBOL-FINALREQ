IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAMRESULT5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE
               ASSIGN TO "OUTFILE5.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUT-FILE.
       01 OUT-REC PIC X(160).

       WORKING-STORAGE SECTION.
       77 VALID-FLAG     PIC 9 VALUE 0.
       77 VALIDANS       PIC 9 VALUE 0.
       77 HEADER-WRITTEN PIC 9 VALUE 0.
       77 PASSED-COUNT   PIC 99 VALUE 0.
       77 FAILED-COUNT   PIC 99 VALUE 0.
       77 ANS            PIC X.

       01 EXAM-REC.
           05 EXAM-NO        PIC 9(10).
           05 EXAM-NAME      PIC X(25).
           05 BIRTH-DATE     PIC X(20).
           05 UNIV-CODE      PIC 9.
           05 UNIV-NAME      PIC X(5).
           05 COURSE-CODE    PIC 9.
           05 COURSE-NAME    PIC X(4).
           05 TOTAL-ITEMS    PIC 9(3).
           05 SCORE          PIC 9(3).
           05 PASS-SCORE     PIC 9(3).
           05 REMARKS        PIC X(6).

       01 INPUT-TEMP.
           05 TOTAL-ITEMS-IN PIC X(5).
           05 SCORE-IN       PIC X(5).
       01 PASSING-DATA.
           05 PASS-PERCENT   PIC 99.

       SCREEN SECTION.
       01 CLRSCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY CLRSCR
           OPEN OUTPUT OUT-FILE
           PERFORM WRITE-HEADER
           PERFORM PROCESS-RTN UNTIL VALIDANS = 1
           PERFORM DISPLAY-TOTALS
           CLOSE OUT-FILE
           STOP RUN.

       PROCESS-RTN.
           DISPLAY CLRSCR
           DISPLAY (1,20) "Professional Regulation Commission"
           DISPLAY (2,23) "IT Professional Board Exam Result"

           DISPLAY (4,1) "Examinee Number: "
           ACCEPT (4,40) EXAM-NO

           DISPLAY (5,1) "Examinee Name: "
           ACCEPT (5,40) EXAM-NAME

           DISPLAY (6,1) "Date of Birth: "
           ACCEPT (6,40) BIRTH-DATE

           PERFORM GET-UNIVERSITY
           DISPLAY (7,1) "University Name: "
           DISPLAY (7,40) UNIV-NAME

           PERFORM GET-COURSE
           DISPLAY (8,1) "Course Name: "
           DISPLAY (8,40) COURSE-NAME

           DISPLAY (9,1) "Total No. of Items: "
           ACCEPT (9,40) TOTAL-ITEMS-IN
           MOVE FUNCTION NUMVAL(TOTAL-ITEMS-IN) TO TOTAL-ITEMS

           DISPLAY (10,1) "Test Result (Score): "
           ACCEPT (10,40) SCORE-IN
           MOVE FUNCTION NUMVAL(SCORE-IN) TO SCORE

           PERFORM CALCULATE-REMARKS
           DISPLAY (11,1) "Remarks: "
           DISPLAY (11,40) REMARKS

           PERFORM UPDATE-TOTALS
           PERFORM WRITE-RECORD

           PERFORM GET-ANSWER
           IF ANS = 'N' OR ANS = 'n'
               MOVE 1 TO VALIDANS
           END-IF.

       GET-UNIVERSITY.
           MOVE 0 TO VALID-FLAG
           PERFORM UNTIL VALID-FLAG = 1
               DISPLAY (7,1) "University Code (1-5): "
               ACCEPT (7,40) UNIV-CODE
               IF UNIV-CODE = 1
                   MOVE "UP" TO UNIV-NAME
                   MOVE 1 TO VALID-FLAG
               ELSE IF UNIV-CODE = 2
                   MOVE "PUP" TO UNIV-NAME
                   MOVE 1 TO VALID-FLAG
               ELSE IF UNIV-CODE = 3
                   MOVE "DLSU" TO UNIV-NAME
                   MOVE 1 TO VALID-FLAG
               ELSE IF UNIV-CODE = 4
                   MOVE "ADMU" TO UNIV-NAME
                   MOVE 1 TO VALID-FLAG
               ELSE IF UNIV-CODE = 5
                   MOVE "MAPUA" TO UNIV-NAME
                   MOVE 1 TO VALID-FLAG
               END-IF
           END-PERFORM.

       GET-COURSE.
           MOVE 0 TO VALID-FLAG
           PERFORM UNTIL VALID-FLAG = 1
               DISPLAY (8,1) "Course Code (1-3): "
               ACCEPT (8,40) COURSE-CODE
               IF COURSE-CODE = 1
                   MOVE "BSIT" TO COURSE-NAME
                   MOVE 1 TO VALID-FLAG
               ELSE IF COURSE-CODE = 2
                   MOVE "BSCS" TO COURSE-NAME
                   MOVE 1 TO VALID-FLAG
               ELSE IF COURSE-CODE = 3
                   MOVE "BSIM" TO COURSE-NAME
                   MOVE 1 TO VALID-FLAG
               END-IF
           END-PERFORM.

       CALCULATE-REMARKS.
           MOVE 0 TO PASS-PERCENT
           EVALUATE COURSE-CODE
               WHEN 1
                   MOVE 60 TO PASS-PERCENT
               WHEN 2
                   MOVE 70 TO PASS-PERCENT
               WHEN 3
                   MOVE 50 TO PASS-PERCENT
           END-EVALUATE
           COMPUTE PASS-SCORE = TOTAL-ITEMS * PASS-PERCENT / 100
           MOVE "FAILED" TO REMARKS
           IF SCORE >= PASS-SCORE
               MOVE "PASSED" TO REMARKS
           END-IF.

       UPDATE-TOTALS.
           IF REMARKS = "PASSED"
               ADD 1 TO PASSED-COUNT
           ELSE
               ADD 1 TO FAILED-COUNT
           END-IF.

       WRITE-RECORD.
           MOVE SPACES TO OUT-REC
           STRING
               EXAM-NO     DELIMITED BY SIZE
               SPACE SPACE
               EXAM-NAME   DELIMITED BY SIZE
               SPACE SPACE
               BIRTH-DATE  DELIMITED BY SIZE
               SPACE SPACE
               UNIV-NAME   DELIMITED BY SIZE
               SPACE SPACE
               COURSE-NAME DELIMITED BY SIZE
               SPACE SPACE
               REMARKS     DELIMITED BY SIZE
           INTO OUT-REC
           END-STRING
           WRITE OUT-REC.

       GET-ANSWER.
           MOVE 0 TO VALID-FLAG
           PERFORM UNTIL VALID-FLAG = 1
               DISPLAY (13,27) "Input Another Record (Y/N)?"
               ACCEPT (13,55) ANS
               IF ANS = 'Y'
                   MOVE 1 TO VALID-FLAG
               ELSE IF ANS = 'y'
                   MOVE 1 TO VALID-FLAG
               ELSE IF ANS = 'N'
                   MOVE 1 TO VALID-FLAG
               ELSE IF ANS = 'n'
                   MOVE 1 TO VALID-FLAG
               ELSE
                   DISPLAY (14,27) "Invalid input. Enter Y or N only"
                   MOVE SPACES TO ANS
               END-IF
           END-PERFORM.

       DISPLAY-TOTALS.
           DISPLAY CLRSCR
           DISPLAY (10,27) "Total No. of Passed: "
           DISPLAY (10,55) PASSED-COUNT
           DISPLAY (11,27) "Total No. of Failed: "
           DISPLAY (11,55) FAILED-COUNT.

       WRITE-HEADER.
           IF HEADER-WRITTEN = 0
               MOVE SPACES TO OUT-REC
               MOVE "Professional Regulation Commission" TO OUT-REC
               WRITE OUT-REC
               MOVE SPACES TO OUT-REC
               MOVE "IT Professional Board Exam Result" TO OUT-REC
               WRITE OUT-REC
               MOVE SPACES TO OUT-REC
               STRING
                   "Examinee No"   DELIMITED BY SIZE
                   SPACE SPACE
                   "Examinee Name" DELIMITED BY SIZE
                   SPACE SPACE
                   "Date of Birth" DELIMITED BY SIZE
                   SPACE SPACE
                   "University"    DELIMITED BY SIZE
                   SPACE SPACE
                   "Course"        DELIMITED BY SIZE
                   SPACE SPACE
                   "Remarks"       DELIMITED BY SIZE
               INTO OUT-REC
               END-STRING
               WRITE OUT-REC
               MOVE 1 TO HEADER-WRITTEN
           END-IF.

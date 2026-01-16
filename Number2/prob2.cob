       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-PROBLEM-2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "D:\h\Prog-Prob2\STUDENT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 OUT-LINE                     PIC X(150).
       
       WORKING-STORAGE SECTION.
      *> Variables
       01 WS-INPUTS.
           05 IN-STUD-NO                PIC 9(10).
           05 IN-STUD-NAME              PIC X(25).
           05 IN-COURSE-CODE            PIC 9.
           05 IN-YEAR                   PIC 9.
           05 IN-SECTION                PIC 9.
           05 IN-STUD-TYPE              PIC X.
           05 IN-TUITION                PIC 9(4)V99.
           05 IN-COUNCIL                PIC 9(3)V99.
           05 IN-LAB                    PIC 9(3)V99.    
           05 IN-MISC                   PIC 9(4)V99.

      *> Calculations / Outputs
       01 WS-CALCULATED.
           05 WS-COURSE-NAME           PIC X(25).
           05 WS-TYPE-NAME             PIC X(10).
           05 WS-TOTAL-FEES            PIC 9(5)V99.
           05 WS-DISP-TOTAL            PIC ZZ,ZZ9.99.
           05 WS-ERR-MSG               PIC X(50).

      *> Header
       01 RPT-TITLE-1.
           05 FILLER                   PIC X(45) VALUE SPACES.
           05 FILLER                   PIC X(50) VALUE 
              "Polytechnic University of the Philippines".
       
       01 RPT-TITLE-2.
           05 FILLER                   PIC X(55) VALUE SPACES.
           05 FILLER                   PIC X(30) VALUE 
              "Sta. Mesa, Manila".

       01 RPT-TITLE-3.
           05 FILLER                   PIC X(50) VALUE SPACES.
           05 FILLER                   PIC X(40) VALUE 
              "Student's Statement of Account".

       01 RPT-COL-HEADERS.
           05 FILLER                   PIC X(15) VALUE "Student No".
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 FILLER                   PIC X(27) VALUE "Student Name".
           05 FILLER                PIC X(20) VALUE "Student Type Name".
           05 FILLER                   PIC X(10) VALUE "Year".
           05 FILLER             PIC X(20) VALUE "Total Amount of Fees".

       01 RPT-DETAIL-LINE.
           05 RPT-STUD-NO              PIC 9(10).
           05 FILLER                   PIC X(7)  VALUE SPACES.
           05 RPT-STUD-NAME            PIC X(25).
           05 FILLER                   PIC X(2)  VALUE SPACES.
           05 RPT-TYPE-NAME            PIC X(10).
           05 FILLER                   PIC X(12) VALUE SPACES.
           05 RPT-YEAR                 PIC 9.
           05 FILLER                   PIC X(12) VALUE SPACES.
           05 RPT-TOTAL-FEES           PIC ZZ,ZZ9.99.

      *> Controls
       01 WS-CONTROLS.
           05 WS-CONTINUE              PIC X VALUE 'Y'.
           05 WS-REC-COUNT             PIC 9 VALUE 0.
           05 WS-VALID-FLAG            PIC X VALUE 'N'.

       SCREEN SECTION.
       01 FORM-LAYOUT.
           05 BLANK SCREEN BACKGROUND-COLOR 1 FOREGROUND-COLOR 7.
           05 LINE 2  COL 25 VALUE 
           "Polytechnic University of the Philippines" 
                               HIGHLIGHT.
           05 LINE 3  COL 35 VALUE "Sta. Mesa, Manila".
           05 LINE 5  COL 30 VALUE "Student's Statement of Account" 
                               HIGHLIGHT.
           05 LINE 6  COL 10 VALUE 
         "------------------------------------------------------------".
           
           05 LINE 8  COL 5  VALUE "Student No (10):".
           05 LINE 9  COL 5  VALUE "Student Name   :".
           05 LINE 11 COL 5  VALUE "Course Code(1-6):".
           05 LINE 11 COL 40 VALUE "Course:".
           05 LINE 12 COL 5  VALUE "Year           :".
           05 LINE 13 COL 5  VALUE "Section        :".
           05 LINE 15 COL 5  VALUE "Type (R/I)     :".
           05 LINE 15 COL 40 VALUE "Type  :".
           05 LINE 17 COL 5  VALUE "Tuition Fee    :".
           05 LINE 18 COL 5  VALUE "Council Fee    :".
           05 LINE 19 COL 5  VALUE "Lab Fee        :".
           05 LINE 20 COL 5  VALUE "Misc Fee       :".
           05 LINE 22 COL 5  VALUE "TOTAL FEES     :" HIGHLIGHT.

       01 F-STUDENT-INFO.
           05 LINE 8  COL 23 PIC 9(10) USING IN-STUD-NO REQUIRED.
           05 LINE 9  COL 23 PIC X(25) USING IN-STUD-NAME REQUIRED.

       01 F-COURSE-CODE.
           05 LINE 11 COL 23 PIC 9 USING IN-COURSE-CODE AUTO.
       01 F-COURSE-NAME.
           05 LINE 11 COL 48 PIC X(25) FROM WS-COURSE-NAME HIGHLIGHT.

       01 F-YEAR.
           05 LINE 12 COL 23 PIC 9 USING IN-YEAR AUTO.
       01 F-SECTION.
           05 LINE 13 COL 23 PIC 9 USING IN-SECTION AUTO.

       01 F-TYPE.
           05 LINE 15 COL 23 PIC X USING IN-STUD-TYPE AUTO.
       01 F-TYPE-NAME.
           05 LINE 15 COL 48 PIC X(10) FROM WS-TYPE-NAME HIGHLIGHT.

       01 F-FEES.
           05 LINE 17 COL 23 PIC 9(7).99 USING IN-TUITION.
           05 LINE 18 COL 23 PIC 9(7).99 USING IN-COUNCIL.
           05 LINE 19 COL 23 PIC 9(7).99 USING IN-LAB.
           05 LINE 20 COL 23 PIC 9(7).99 USING IN-MISC.

       01 F-TOTAL-DISP.
        05 LINE 22 COL 23 PIC ZZ,ZZ9.99 FROM WS-DISP-TOTAL HIGHLIGHT.

       01 F-ERROR-MSG.
           05 LINE 24 COL 5 PIC X(50) FROM WS-ERR-MSG 
              BACKGROUND-COLOR 4 FOREGROUND-COLOR 7 BLANK LINE.
       
       01 F-CONTINUE-MSG.
           05 LINE 24 COL 5 VALUE "Input another record (Y/N)? " 
              BACKGROUND-COLOR 1 FOREGROUND-COLOR 7 BLANK LINE.
           05 LINE 24 COL 35 PIC X TO WS-CONTINUE AUTO REQUIRED.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN OUTPUT STUDENT-FILE.
           
           PERFORM WRITE-FILE-HEADERS.

           PERFORM UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n' OR     
               WS-REC-COUNT = 5
               
               INITIALIZE WS-INPUTS WS-CALCULATED
               MOVE SPACES TO WS-ERR-MSG
               DISPLAY FORM-LAYOUT
               PERFORM GET-AND-PROCESS-INPUTS
               PERFORM WRITE-TO-FILE-DETAIL

               ADD 1 TO WS-REC-COUNT

               IF WS-REC-COUNT < 5
                   PERFORM ASK-CONTINUE
               ELSE 
                   MOVE "Max limit of 5 records reached. Press Enter..." 
                     TO WS-ERR-MSG
                   DISPLAY F-ERROR-MSG
                   ACCEPT WS-CONTINUE
               END-IF
           END-PERFORM
           
           CLOSE STUDENT-FILE.
           STOP RUN.

       GET-AND-PROCESS-INPUTS.
      *> (Logic remains the same as previous step, omitted for brevity)
           ACCEPT F-STUDENT-INFO.

           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               ACCEPT F-COURSE-CODE
               IF IN-COURSE-CODE >= 1 AND IN-COURSE-CODE <= 6
                   MOVE 'Y' TO WS-VALID-FLAG
                   MOVE SPACES TO WS-ERR-MSG
                   DISPLAY F-ERROR-MSG
                   EVALUATE IN-COURSE-CODE
                       WHEN 1 MOVE "Accounting" TO WS-COURSE-NAME
                       WHEN 2 MOVE "Arts" TO WS-COURSE-NAME
                       WHEN 3 MOVE "Business" TO WS-COURSE-NAME
                       WHEN 4 MOVE "Comp Sci/IT" TO WS-COURSE-NAME
                       WHEN 5 MOVE "Education" TO WS-COURSE-NAME
                       WHEN 6 MOVE "Engineering" TO WS-COURSE-NAME
                   END-EVALUATE
                   DISPLAY F-COURSE-NAME
               ELSE
              MOVE "ERROR: Valid Course Codes are 1 to 6." TO WS-ERR-MSG
                   DISPLAY F-ERROR-MSG
               END-IF
           END-PERFORM.

           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               ACCEPT F-YEAR
               IF IN-COURSE-CODE = 6
                   IF IN-YEAR >= 1 AND IN-YEAR <= 5
                       MOVE 'Y' TO WS-VALID-FLAG
                       MOVE SPACES TO WS-ERR-MSG
                       DISPLAY F-ERROR-MSG
                   ELSE 
                  MOVE "ERROR: Engineering years are 1-5." TO WS-ERR-MSG
                       DISPLAY F-ERROR-MSG
                   END-IF
               ELSE
                   IF IN-YEAR >= 1 AND IN-YEAR <= 4
                       MOVE 'Y' TO WS-VALID-FLAG
                       MOVE SPACES TO WS-ERR-MSG
                       DISPLAY F-ERROR-MSG
                   ELSE
                       MOVE "ERROR: Valid years are 1-4." TO WS-ERR-MSG
                       DISPLAY F-ERROR-MSG
                   END-IF
               END-IF
           END-PERFORM.

           ACCEPT F-SECTION.

           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               ACCEPT F-TYPE
               IF IN-STUD-TYPE = 'R' OR 'r' OR 'I' OR 'i'
                   MOVE 'Y' TO WS-VALID-FLAG
                   MOVE SPACES TO WS-ERR-MSG
                   DISPLAY F-ERROR-MSG
                   IF IN-STUD-TYPE = 'R' OR IN-STUD-TYPE = 'r'
                       MOVE "Regular" TO WS-TYPE-NAME
                   ELSE
                       MOVE "Irregular" TO WS-TYPE-NAME
                   END-IF
                   DISPLAY F-TYPE-NAME
               ELSE
                   MOVE "ERROR: Enter R (Regular) or I (Irregular)." 
                     TO WS-ERR-MSG
                   DISPLAY F-ERROR-MSG
               END-IF
           END-PERFORM.

           ACCEPT F-FEES.
           COMPUTE WS-TOTAL-FEES = IN-TUITION + IN-COUNCIL + IN-LAB + 
               IN-MISC.
           MOVE WS-TOTAL-FEES TO WS-DISP-TOTAL.
           DISPLAY F-TOTAL-DISP.

       ASK-CONTINUE.
           MOVE 'N' TO WS-VALID-FLAG
           MOVE SPACE TO WS-CONTINUE
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY F-CONTINUE-MSG
               ACCEPT F-CONTINUE-MSG
               IF WS-CONTINUE = 'Y' OR 'y' OR 'N' OR 'n'
                   MOVE 'Y' TO WS-VALID-FLAG
               ELSE
                   MOVE "ERROR: Please Input Only Y or N" TO WS-ERR-MSG
                   DISPLAY F-ERROR-MSG
               END-IF
           END-PERFORM.

       WRITE-FILE-HEADERS.
      *> Writes the top portion of the report to the file
           WRITE OUT-LINE FROM RPT-TITLE-1.
           WRITE OUT-LINE FROM RPT-TITLE-2.
           MOVE SPACES TO OUT-LINE.
           WRITE OUT-LINE.
           WRITE OUT-LINE FROM RPT-TITLE-3.
           MOVE SPACES TO OUT-LINE.
           WRITE OUT-LINE.
           WRITE OUT-LINE FROM RPT-COL-HEADERS.
           MOVE SPACES TO OUT-LINE.
           WRITE OUT-LINE.

       WRITE-TO-FILE-DETAIL.
      *> Formats the data into the detail line and writes it
           MOVE IN-STUD-NO   TO RPT-STUD-NO.
           MOVE IN-STUD-NAME TO RPT-STUD-NAME.
           MOVE WS-TYPE-NAME TO RPT-TYPE-NAME.
           MOVE IN-YEAR      TO RPT-YEAR.
           MOVE WS-TOTAL-FEES TO RPT-TOTAL-FEES.
           
           WRITE OUT-LINE FROM RPT-DETAIL-LINE.

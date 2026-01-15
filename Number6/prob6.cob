       IDENTIFICATION DIVISION.
       PROGRAM-ID. POPULATION-REPORT.
       AUTHOR. GROUP WORK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POPUL-FILE ASSIGN TO "D:\h\Prog-Prob6\POPUL.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD POPUL-FILE.
       01 OUTREC               PIC X(80).

       WORKING-STORAGE SECTION.
       01 INPUT-DATA.
           05 BCODE-IN         PIC 9(2).
               88 VALID-RANGE  VALUE 1 THRU 4.
           05 NUMCOURSE-IN     PIC 9(2).
           05 NUMREGFAC-IN     PIC 9(3).
           05 NUMPTFAC-IN      PIC 9(3).
           05 NUMSTUD-IN       PIC 9(6).
           05 NUMREG-IN        PIC 9(6).
       01 TRACKER-TABLE.
           05 NUM-STATUS       PIC X OCCURS 4 TIMES VALUE 'N'.
               88 IS-USED          VALUE 'Y'.
               88 IS-AVAILABLE     VALUE 'N'.
       01 COMPUTE-DATA.
           05 LOCNAME          PIC X(20).
           05 NUMIRREG         PIC 9(6).
           05 TOTFAC           PIC 9(6).
           05 LARGESTUD        PIC 9(6) VALUE 0.
           05 LARGEFAC         PIC 9(6) VALUE 0.
           05 ERR-MSG          PIC X(50).
       
       01 TEMP-DATA.
           05 LSTUDBRANCH      PIC X(20).
           05 LFACBRANCH       PIC X(20).
       
       01 CHECK-CONT.
           05 EOFSW           PIC X VALUE 'Y'.
           05 FLAGSW          PIC X VALUE 'N'.
       
       01 H1.
           05 FILLER           PIC X(19) VALUE SPACES.
           05 FILLER           PIC X(41) VALUE 
                     "Polytechnic University of the Philippines".
           05 FILLER           PIC X(20) VALUE SPACES.
       01 H2.
           05 FILLER           PIC X(31) VALUE SPACES.
           05 FILLER           PIC X(17) VALUE "Sta. Mesa, Manila".
           05 FILLER           PIC X(32) VALUE SPACES.   
       01 H3.
           05 FILLER           PIC X(31) VALUE SPACES.
           05 FILLER           PIC X(17) VALUE "Population Report".
           05 FILLER           PIC X(32) VALUE SPACES.
       01  H4.
           05 FILLER           PIC X(33) VALUE SPACES.
           05 FILLER           PIC X(14) VALUE "First Semester".
           05 FILLER           PIC X(33) VALUE SPACES.
       01 H5.
           05 FILLER           PIC X(35) VALUE SPACES.
           05 FILLER           PIC X(9) VALUE "2010-2011".
           05 FILLER           PIC X(36) VALUE SPACES.

       01 SH1.
           05 FILLER           PIC X(9) VALUE SPACES.
           05 FILLER           PIC X(8) VALUE "LOCATION".
           05 FILLER           PIC X(9) VALUE SPACES.
           05 FILLER           PIC X(9) VALUE "TOTAL NO.".
           05 FILLER           PIC X(9) VALUE SPACES.
           05 FILLER           PIC X(9) VALUE "TOTAL NO.".
           05 FILLER           PIC X(9) VALUE SPACES.
           05 FILLER           PIC X(9) VALUE "TOTAL NO.".
           05 FILLER           PIC X(9) VALUE SPACES.
       01 SH2.
           05 FILLER           PIC X(11) VALUE SPACES.
           05 FILLER           PIC X(4) VALUE "NAME".
           05 FILLER           PIC X(12) VALUE SPACES.
           05 FILLER           PIC X(7) VALUE "COURSES".
           05 FILLER           PIC X(6) VALUE SPACES.
           05 FILLER           PIC X(17) VALUE "ENROLLED STUDENTS".
           05 FILLER           PIC X(6) VALUE SPACES.
           05 FILLER           PIC X(7) VALUE "FACULTY".
           05 FILLER           PIC X(10) VALUE SPACES.
       
       01 REC-OUT.
           05 FILLER           PIC X(6) VALUE SPACES.
           05 LOCNAME-OUT      PIC X(20).
           05 FILLER           PIC X(4) VALUE SPACES.
           05 NUMCOURSE-OUT    PIC 9(2).
           05 FILLER           PIC X(14) VALUE SPACES.
           05 NUMSTUD-OUT      PIC ZZZ,ZZ9.
           05 FILLER           PIC X(11) VALUE SPACES.
           05 TOTFAC-OUT       PIC ZZZ,ZZ9.
           05 FILLER           PIC X(11) VALUE SPACES.
       
       SCREEN SECTION.
       01 HEADER.
           05 BLANK SCREEN.
           05 LINE 1 COL 20 VALUE 
               "Polytechnic University of the Philippines".
           05 LINE 2 COL 32 VALUE "Sta. Mesa, Manila".
           05 LINE 5 COL 32 VALUE "Population Report".
           05 LINE 6 COL 34 VALUE "First Semester".
           05 LINE 7 COL 36 VALUE "2010-2011".
       01 LAYOUT.
           05 LINE 9 COL 1 VALUE "Location Branch Code: ".
           05 LINE 10 COL 1 VALUE "Location Name: ".
           05 LINE 11 COL 1 VALUE "Total No. of Courses Offered: ".
           05 LINE 12 COL 1 VALUE "Total No. of Regular Faculty: ".
           05 LINE 13 COL 1 VALUE "Total No. of Part Time Faculty: ".
           05 LINE 14 COL 1 VALUE "Total No. of Enrolled Students: ".
           05 LINE 15 COL 1 VALUE "Total No. of Regular Students: ".
           05 LINE 16 COL 1 VALUE "Total No. of Irregular Students: ".
       01 SCR-BCODE.
           05 LINE 9 COL 45 PIC 9(2) USING BCODE-IN REQUIRED AUTO.
       01  SCR-LOC-NAME.
           05 LINE 10 COL 45 PIC X(20) USING LOCNAME HIGHLIGHT.
       01 SCR-INPUT.
           05 LINE 11 COL 45 PIC 9(2) USING NUMCOURSE-IN REQUIRED AUTO.
           05 LINE 12 COL 45 PIC 9(3) USING NUMREGFAC-IN REQUIRED AUTO.
           05 LINE 13 COL 45 PIC 9(3) USING NUMPTFAC-IN REQUIRED AUTO.
           05 LINE 14 COL 45 PIC 9(6) USING NUMSTUD-IN REQUIRED AUTO.
       01 SCR-REG.
           05 LINE 15 COL 45 PIC 9(6) USING NUMREG-IN REQUIRED AUTO.
       01 SCR-IRREG.
           05 LINE 16 COL 45 PIC 9(6) USING NUMIRREG HIGHLIGHT.
       01 SCR-CONTINUE.
           05 LINE 18 COL 1 VALUE "Input Another Record (Y/N)?:       ".
           05 LINE 18 COL 36 VALUE "          ".
           05 LINE 18 COL 47 PIC X USING EOFSW REQUIRED AUTO.
       01 SCR-ERROR.
           05 LINE 18 COL 1 FROM ERR-MSG.
       01 SCR-FINAL.
           05 LINE 20 COL 1 VALUE "Largest No. of Enrolled Students: ".
           05 LINE 20 COL 45 PIC ZZZ,ZZ9 FROM LARGESTUD.
           05 LINE 21 COL 1 VALUE "Branch Name: ".
           05 LINE 21 COL 45 PIC X(20) FROM LSTUDBRANCH.
           05 LINE 24 COL 1 VALUE "Largest No. of Faculty: ".
           05 LINE 24 COL 45 PIC ZZZ,ZZ9 FROM LARGEFAC.
           05 LINE 25 COL 1 VALUE "Branch Name: ".
           05 LINE 25 COL 45 PIC X(20) FROM LFACBRANCH.
       
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INITIAL-RTN

           PERFORM PROCESS-RTN UNTIL EOFSW = 'N' OR 'n'

           PERFORM FINISH-RTN
           STOP RUN.
       INITIAL-RTN.
           OPEN OUTPUT POPUL-FILE
           WRITE OUTREC FROM H1
           WRITE OUTREC FROM H2
           WRITE OUTREC FROM SPACES
           WRITE OUTREC FROM H3
           WRITE OUTREC FROM H4
           WRITE OUTREC FROM H5
           WRITE OUTREC FROM SPACES
           WRITE OUTREC FROM SPACES
           WRITE OUTREC FROM SH1
           WRITE OUTREC FROM SH2.
           
       PROCESS-RTN.
           DISPLAY HEADER
           DISPLAY LAYOUT

           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-BCODE
               ACCEPT SCR-BCODE
               EVALUATE TRUE
                   WHEN NOT VALID-RANGE
                     MOVE "ERROR: Numbers 1-4 Only." TO ERR-MSG
                     DISPLAY SCR-ERROR

                   WHEN IS-USED(BCODE-IN) 
                     MOVE "ERROR: No. is already Used!" TO ERR-MSG
                     DISPLAY SCR-ERROR

                   WHEN VALID-RANGE AND IS-AVAILABLE(BCODE-IN)
                     MOVE 'Y' TO FLAGSW
                     MOVE 'Y' TO NUM-STATUS(BCODE-IN)
                     MOVE SPACES TO ERR-MSG
                     DISPLAY SCR-ERROR

                     EVALUATE BCODE-IN
                        WHEN 1 MOVE "PUP Main" TO LOCNAME
                        WHEN 2 MOVE "PUP Commonwealth" TO LOCNAME
                        WHEN 3 MOVE "PUP Sta. Rosa" TO LOCNAME
                        WHEN 4 MOVE "PUP Taguig" TO LOCNAME
                     END-EVALUATE 
                      
                      DISPLAY SCR-LOC-NAME

               END-EVALUATE  
           END-PERFORM

           DISPLAY SCR-INPUT
           ACCEPT SCR-INPUT

           MOVE 'N' TO FLAGSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-REG
               ACCEPT SCR-REG

               IF NUMREG-IN > NUMSTUD-IN
                   MOVE "ERROR: Regular Students Exceeded Total" 
                       TO ERR-MSG
                   DISPLAY SCR-ERROR
               ELSE   
                   MOVE 'Y' TO FLAGSW
               END-IF 

           END-PERFORM

           COMPUTE NUMIRREG = NUMSTUD-IN - NUMREG-IN
           DISPLAY SCR-IRREG

           COMPUTE TOTFAC = NUMREGFAC-IN + NUMPTFAC-IN

           IF LARGESTUD < NUMREG-IN
               MOVE NUMREG-IN TO LARGESTUD
               MOVE LOCNAME TO LSTUDBRANCH
           END-IF

           IF LARGEFAC < TOTFAC
               MOVE TOTFAC TO LARGEFAC
               MOVE LOCNAME TO LFACBRANCH
           END-IF 

           MOVE LOCNAME TO LOCNAME-OUT
           MOVE NUMCOURSE-IN TO NUMCOURSE-OUT
           MOVE NUMSTUD-IN TO NUMSTUD-OUT
           MOVE TOTFAC TO TOTFAC-OUT
           WRITE OUTREC FROM REC-OUT
           
           MOVE 'N' TO FLAGSW
           MOVE SPACE TO EOFSW
           PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY SCR-CONTINUE
               ACCEPT SCR-CONTINUE
               IF EOFSW = 'Y' OR 'y' OR 'N' OR 'n'
                   MOVE 'Y' TO FLAGSW
                   MOVE SPACES TO ERR-MSG
                   DISPLAY SCR-ERROR
               ELSE 
                   MOVE "ERROR: Y or N Only" TO SCR-ERROR
                   DISPLAY ERROR
               END-IF 
           END-PERFORM.       
       FINISH-RTN.
           DISPLAY SCR-FINAL
           CLOSE POPUL-FILE.

           
              

           
       

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BROWNOUT-BILLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLING-FILE ASSIGN TO "D:\h\Prog-Prob4\BILLINGS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD BILLING-FILE.
       01 OUTREC              PIC X(150).

       WORKING-STORAGE SECTION.
      *---------------- INPUT FIELDS ----------------*
       01  WS-ACCOUNT-NO        PIC X(10).
       01  WS-CUST-NAME         PIC X(25).
       01  WS-PREV-READ         PIC 9(6).
       01  WS-CURR-READ         PIC 9(6).
       01  WS-KWH-USED          PIC 9(6).
       01  WS-ACCOUNT-CODE      PIC X.
       01  WS-ACCOUNT-TYPE      PIC X(10).
       01  WS-AREA-CODE         PIC 9.

      *---------------- COMPUTATION ----------------*
       01  WS-PRICE-PER-KWH     PIC 9(3).
       01  WS-ELECTRIC-BILL     PIC 9(7)V99.
       01  WS-SYSTEM-CHARGES    PIC 9(7)V99.
       01  WS-TOTAL-BILL        PIC 9(7)V99.

      *---------------- FORMATTED OUTPUT ----------------*
       01  WS-SYS-CHG-DISP      PIC Z,ZZZ.ZZ.
       01  WS-TOTAL-DISP        PIC ZZ,ZZZ.ZZ.

      *---------------- CONTROL ----------------*
       01  WS-ANSWER            PIC X.
       01  WS-MAX-KWH           PIC 9(6) VALUE 0.
       01  WS-MAX-CUST          PIC X(25).
       01  FLAGSW               PIC X VALUE 'N'.

       01 H1.
           05 FILLER            PIC X(27) VALUE SPACES. 
           05 FILLER            PIC X(25) VALUE 
                                   "BROWNOUT ELECTRIC COMPANY".
           05 FILLER            PIC X(28) VALUE SPACES.       
       01 H2.
           05 FILLER            PIC X(33) VALUE SPACES.
           05 FILLER            PIC X(14) VALUE "BILLING REPORT".
           05 FILLER            PIC X(33) VALUE SPACES.
       01 SH1.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(07) VALUE "ACCOUNT".
           05 FILLER            PIC X(03) VALUE SPACES.
           05 FILLER            PIC X(09) VALUE SPACES.
           05 FILLER            PIC X(08) VALUE "CUSTOMER".
           05 FILLER            PIC X(10) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(07) VALUE "ACCOUNT".
           05 FILLER            PIC X(03) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(03) VALUE "KWH".
           05 FILLER            PIC X(03) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(06) VALUE "SYSTEM".
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(03) VALUE SPACES.
           05 FILLER            PIC X(05) VALUE "TOTAL".
           05 FILLER            PIC X(03) VALUE SPACES.

       01 SH2.
           05 FILLER            PIC X(05) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE "NO".
           05 FILLER            PIC X(05) VALUE SPACES.
           05 FILLER            PIC X(11) VALUE SPACES.
           05 FILLER            PIC X(04) VALUE "NAME".
           05 FILLER            PIC X(12) VALUE SPACES.
           05 FILLER            PIC X(04) VALUE SPACES.
           05 FILLER            PIC X(04) VALUE "TYPE".
           05 FILLER            PIC X(04) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(04) VALUE "USED".
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(01) VALUE SPACES.
           05 FILLER            PIC X(07) VALUE "CHARGES".
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(03) VALUE SPACES.
           05 FILLER            PIC X(04) VALUE "BILL".
           05 FILLER            PIC X(04) VALUE SPACES.

       01 REC-OUT.
           05 FILLER            PIC X(01) VALUE SPACES.
           05 ACCNO-OUT         PIC X(10).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 ACCNAME-OUT       PIC X(25).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 ACCTYPE-OUT       PIC X(10).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 KWH-OUT           PIC Z(6).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 SYSCHAR-OUT       PIC Z,ZZZ.ZZ.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 TOTBILL-OUT       PIC ZZ,ZZZ.ZZ.
           05 FILLER            PIC X(01) VALUE SPACES.

       SCREEN SECTION.
       01 CLRSCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INITIAL-RTN

           PERFORM PROCESS-RTN UNTIL WS-ANSWER = 'N'

           PERFORM FINISH-RTN
           STOP RUN.

       INITIAL-RTN.    
           OPEN OUTPUT BILLING-FILE
           WRITE OUTREC FROM H1
           WRITE OUTREC FROM H2
           WRITE OUTREC FROM SPACES
           WRITE OUTREC FROM SH1
           WRITE OUTREC FROM SH2
           WRITE OUTREC FROM REC-OUT.

       PROCESS-RTN.
          DISPLAY CLRSCR

          DISPLAY (1, 1) "----------------------------------------"
          DISPLAY (2, 7) "BROWNOUT ELECTRIC COMPANY"
          DISPLAY (3, 13) "BILLING REPORT"
          DISPLAY (4, 1) "----------------------------------------"

          DISPLAY (6, 1) "Account Number: "
          ACCEPT (6, 25) WS-ACCOUNT-NO

          DISPLAY (8, 1) "Customer Name: "
          ACCEPT (8, 25) WS-CUST-NAME

          MOVE 'N' TO FLAGSW
          PERFORM KWH-USED-RTN UNTIL FLAGSW = 'Y'
         
               
          MOVE 'N' TO FLAGSW
          PERFORM ACC-CODE-RTN UNTIL FLAGSW = 'Y'

          DISPLAY (13, 1) "                     "
          DISPLAY (13,1) "Account Type: "
          DISPLAY (13, 25) WS-ACCOUNT-TYPE 

          MOVE 'N' TO FLAGSW
          PERFORM AREA-CODE-RTN UNTIL FLAGSW = 'Y'

          COMPUTE WS-TOTAL-BILL = WS-ELECTRIC-BILL + WS-SYSTEM-CHARGES
               
          MOVE WS-SYSTEM-CHARGES TO WS-SYS-CHG-DISP
          MOVE WS-TOTAL-BILL TO WS-TOTAL-DISP

          DISPLAY (15, 1) "System Charges: "
          DISPLAY (15, 25) WS-SYS-CHG-DISP

          DISPLAY (16,1) "Total Bill: "
          DISPLAY (16, 25) WS-TOTAL-DISP

          IF WS-KWH-USED > WS-MAX-KWH
               MOVE WS-KWH-USED TO WS-MAX-KWH
               MOVE WS-CUST-NAME TO WS-MAX-CUST
          END-IF

          MOVE WS-ACCOUNT-NO TO ACCNO-OUT
          MOVE WS-CUST-NAME TO ACCNAME-OUT
          MOVE WS-ACCOUNT-TYPE TO ACCTYPE-OUT
          MOVE WS-KWH-USED TO KWH-OUT
          MOVE WS-SYSTEM-CHARGES TO SYSCHAR-OUT
          MOVE WS-TOTAL-BILL TO TOTBILL-OUT
          WRITE OUTREC FROM REC-OUT

          MOVE 'N' TO FLAGSW
          PERFORM UNTIL FLAGSW = 'Y'
               DISPLAY (17, 1) "Input Another Record (Y/N): "
               ACCEPT (17, 28) WS-ANSWER
               MOVE FUNCTION UPPER-CASE(WS-ANSWER) TO WS-ANSWER

               IF WS-ANSWER = 'Y' OR = 'N'
                   DISPLAY (18, 1) "                    "
                   MOVE 'Y' TO FLAGSW
               ELSE
                   DISPLAY (18, 1) "ERROR: Invalid Input"
               END-IF

           END-PERFORM.

       KWH-USED-RTN.
           DISPLAY (9, 1) "Previous Reading: "
           ACCEPT (9, 25) WS-PREV-READ
    
           DISPLAY (10, 1) "Current Reading: "
           ACCEPT (10, 25) WS-CURR-READ
    
           IF WS-PREV-READ > WS-CURR-READ
               DISPLAY (11, 1) "ERROR: Previous Head Overload"
           ELSE 
               COMPUTE WS-KWH-USED = WS-CURR-READ - WS-PREV-READ
               DISPLAY (11, 1) "                             "
               DISPLAY (11, 1) "KwH Used: "
               DISPLAY (11, 25) WS-KWH-USED
               MOVE 'Y' TO FLAGSW
           END-IF.
       
       ACC-CODE-RTN.
           DISPLAY (12, 1) "Account Code (R/C/I): "
           ACCEPT (12, 25) WS-ACCOUNT-CODE
           MOVE FUNCTION UPPER-CASE(WS-ACCOUNT-CODE) TO WS-ACCOUNT-CODE

           EVALUATE WS-ACCOUNT-CODE
               WHEN 'R'
                   MOVE "Residential" TO WS-ACCOUNT-TYPE
                   MOVE 14 TO WS-PRICE-PER-KWH
                   MOVE 'Y' TO FLAGSW
               WHEN 'C'
                   MOVE "Commercial" TO WS-ACCOUNT-TYPE
                   MOVE 28 TO WS-PRICE-PER-KWH
                   MOVE 'Y' TO FLAGSW
               WHEN 'I'
                   MOVE "Industrial" TO WS-ACCOUNT-TYPE
                   MOVE 42 TO WS-PRICE-PER-KWH
                   MOVE 'Y' TO FLAGSW
               WHEN OTHER
                   DISPLAY (13, 1) "ERROR: Invalid Input!"
           END-EVALUATE.
       
       AREA-CODE-RTN.
           DISPLAY (14, 1) "Area Code (1/2/3): "
           ACCEPT (14, 25) WS-AREA-CODE

           IF WS-AREA-CODE > 3 OR WS-AREA-CODE < 1
               DISPLAY (15, 1) "ERROR: Invalid Input!"
           ELSE 
               COMPUTE WS-ELECTRIC-BILL = WS-KWH-USED * 
                                         WS-PRICE-PER-KWH
               EVALUATE WS-AREA-CODE
                   WHEN 1
                       COMPUTE WS-SYSTEM-CHARGES =
                                   WS-ELECTRIC-BILL * 0.03
                   WHEN 2
                       COMPUTE WS-SYSTEM-CHARGES =
                                   WS-ELECTRIC-BILL * 0.05
                   WHEN 3
                       COMPUTE WS-SYSTEM-CHARGES =
                                   WS-ELECTRIC-BILL * 0.07
                   END-EVALUATE

                   MOVE 'Y' TO FLAGSW
                   DISPLAY (15, 1) "                     "
               END-IF.

       FINISH-RTN.
           DISPLAY (19, 1) "========================================="
           DISPLAY (20, 1)"Customer with Highest KWH Used:"
           DISPLAY (20, 34) WS-MAX-CUST
           DISPLAY (21, 1) "========================================="
           
           CLOSE BILLING-FILE.

      
       

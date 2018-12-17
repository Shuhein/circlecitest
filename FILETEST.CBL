000100* T   �t�@�C�����͐��\����:�f�[�^�x�[�X�̑S�����o�i�p�X�������j
000200 IDENTIFICATION         DIVISION.
000300 PROGRAM-ID.            FILETEST.
000400* PATTERN-ID.           COLATN01.
000500*AUTHOR.                HITACHI.
000600*DATE-WRITTEN.
000700*DATE-COMPILED.
000800**
000900 ENVIRONMENT            DIVISION.
001000 CONFIGURATION          SECTION.
001100 SOURCE-COMPUTER.       HITAC.
001200*OBJECT-COMPUTER.       HITAC.
001300 SPECIAL-NAMES.
001400                        C01       IS   PCHANGE.
001500 INPUT-OUTPUT           SECTION.
001600 FILE-CONTROL.
001700     SELECT   FILE-A
001800               ASSIGN TO INPUTDATA1.
001900     SELECT   FILE-B
002000               ASSIGN TO INPUTDATA2.
002100     SELECT   FILE-C
002200               ASSIGN TO INPUTDATA3.
002300     SELECT   FILE-OUT-REC
002400               ASSIGN TO OUTPUTDATA
002500               ORGANIZATION IS LINE SEQUENTIAL.
002600     SELECT   FILE-OUT-STATUS
002700               ASSIGN TO EXESTATUS
002800               ORGANIZATION IS LINE SEQUENTIAL.
002900
003000 DATA                   DIVISION.
003100 FILE                   SECTION.
003200 FD  FILE-A.
003300     COPY  ROOT     PREFIXING  IA-.
003400
003500 FD  FILE-B.
003600     COPY  SUB      PREFIXING  IB-.
003700
003800 FD  FILE-C.
003900     COPY  SUB2     PREFIXING  IC-.
004000
004100 FD  FILE-OUT-REC.
004200     COPY  OUTFILE  PREFIXING  O1-.
004300
004400 FD  FILE-OUT-STATUS.
004500     COPY  RESULT   PREFIXING  O2-.
004600
004700 WORKING-STORAGE        SECTION.
004800******************************************************************
004900*                                                                *
005000*    ���͍�Ɨ̈�                                                *
005100*                                                                *
005200******************************************************************
005300*D   FILE-A ���[�N�G���A
005400 COPY  ROOT  PREFIXING  WA-.
005500 01  WA-COUNTER    PIC  S9(11)  VALUE  ZERO.
005600 01  WA-EOF-FLG    PIC   X      VALUE  ZERO.
005700******************************************************************
005800*                                                                *
005900*    ���͍�Ɨ̈�                                                *
006000*                                                                *
006100******************************************************************
006200*D   FILE-B ���[�N�G���A
006300 COPY  SUB  PREFIXING  WB-.
006400 01  WB-COUNTER    PIC  S9(11)  VALUE  ZERO.
006500 01  WB-EOF-FLG    PIC   X      VALUE  ZERO.
006600******************************************************************
006700*                                                                *
006800*    ���͍�Ɨ̈�                                                *
006900*                                                                *
007000******************************************************************
007100*D   FILE-C ���[�N�G���A
007200 COPY  SUB2  PREFIXING  WC-.
007300 01  WC-COUNTER    PIC  S9(11)  VALUE  ZERO.
007400 01  WC-EOF-FLG    PIC   X      VALUE  ZERO.
007500******************************************************************
007600*                                                                *
007700*    �o�͍�Ɨ̈�                                            *
007800*                                                                *
007900******************************************************************
008000*D   FILE-OUT-REC ���[�N�G���A
008100 COPY  OUTFILE  PREFIXING  W1-.
008200 01  W1-COUNTER    PIC  S9(11)  VALUE  ZERO.
008300 01  W1-EOF-FLG    PIC   X      VALUE  ZERO.
008400******************************************************************
008500*                                                                *
008600*    �o�͍�Ɨ̈�                                                *
008700*                                                                *
008800******************************************************************
008900*D   FILE-OUT-STATUS ���[�N�G���A
009000 COPY  RESULT  PREFIXING  W2-.
009100 01  W2-COUNTER    PIC  S9(11)  VALUE  ZERO.
009200 01  W2-EOF-FLG    PIC   X      VALUE  ZERO.
009300
009400 PROCEDURE              DIVISION.
009500*
009600******************************************************************
009700*                                                                *
009800*G   ���C������                                                  *
009900*                                                                *
010000******************************************************************
010100 MAIN-PROC                         SECTION.
010200*G   ���C��
010300 MAIN-PROC-010.
010400
010500     PERFORM  JUNBI-PROC.
010600
010700     OPEN  INPUT   FILE-A
010800                   FILE-B
010900                   FILE-C.
011000     OPEN  OUTPUT  FILE-OUT-REC.
011100
011200     PERFORM  ROOT-READ-PROC.
011300     PERFORM  SUB-READ-PROC.
011400     PERFORM  SUB2-READ-PROC.
011500
011600     PERFORM UNTIL WA-EOF-FLG = "1"
011700
011800         PERFORM ROOT-WRITE-PROC
011900
012000         PERFORM UNTIL ( WB-EOF-FLG  = "1" OR
012100                         WB-ROOT-KEY NOT = WA-ROOT-KEY )
012200
012300             PERFORM SUB-WRITE-PROC
012400
012500             PERFORM UNTIL (WC-EOF-FLG = "1" OR
012600                            WC-SUB-KEY NOT = WB-SUB-KEY )
012700
012800               PERFORM  SUB2-WRITE-PROC
012900               PERFORM  SUB2-READ-PROC
013000
013100             END-PERFORM
013200
013300             PERFORM  SUB-READ-PROC
013400
013500         END-PERFORM
013600
013700         PERFORM  ROOT-READ-PROC
013800
013900     END-PERFORM.
014000
014100     CLOSE  FILE-A
014200            FILE-B
014300            FILE-C.
014400     CLOSE  FILE-OUT-REC.
014500
014600     PERFORM SYURYO-PROC.
014700
014800     STOP     RUN.
014900                                                             SKIP3
015000******************************************************************
015100*                                                                *
015200*    ROOT���͏���                                                *
015300*                                                                *
015400******************************************************************
015500 ROOT-READ-PROC SECTION.
015600*G   ROOT ���͏���
015700 ROOT-READ-010.
015800     READ  FILE-A
015900       AT END
016000         MOVE "1" TO WA-EOF-FLG
016100       NOT AT END
016200         ADD 1 TO WA-COUNTER
016300         MOVE IA-ROOT TO WA-ROOT
016400     END-READ.
016500 ROOT-READ-999.
016600 EXIT.
016700                                                             SKIP3
016800******************************************************************
016900*                                                                *
017000*    SUB���͏���                                                 *
017100*                                                                *
017200******************************************************************
017300 SUB-READ-PROC SECTION.
017400*G   SUB ���͏���
017500 SUB-READ-010.
017600     READ  FILE-B
017700       AT END
017800         MOVE "1" TO WB-EOF-FLG
017900       NOT AT END
018000         ADD 1 TO WB-COUNTER
018100         MOVE IB-SUB TO WB-SUB
018200     END-READ.
018300 SUB-READ-999.
018400 EXIT.
018500                                                             SKIP3
018600******************************************************************
018700*                                                                *
018800*    SUB2���͏���                                                *
018900*                                                                *
019000******************************************************************
019100 SUB2-READ-PROC SECTION.
019200*G   SUB ���͏���
019300 SUB2-READ-010.
019400     READ  FILE-C
019500       AT END
019600         MOVE "1" TO WC-EOF-FLG
019700       NOT AT END
019800         ADD 1 TO WC-COUNTER
019900         MOVE IC-SUB2 TO WC-SUB2
020000     END-READ.
020100 SUB2-READ-999.
020200 EXIT.
020300                                                             SKIP3
020400******************************************************************
020500*                                                                *
020600*    ROOT�o�͏���                                                *
020700*                                                                *
020800******************************************************************
020900 ROOT-WRITE-PROC SECTION.
021000*G   SUB ���͏���
021100 ROOT-WRITE-010.
021200     INITIALIZE O1-OUTFILE.
021300     MOVE WA-ROOT-KEY TO W1-ROOT-KEY.
021400     MOVE W1-OUTFILE TO O1-OUTFILE.
021500     WRITE O1-OUTFILE
021600     END-WRITE.
021700     INITIALIZE W1-OUTFILE.
021800 ROOT-WRITE-999.
021900 EXIT.
022000                                                             SKIP3
022100******************************************************************
022200*                                                                *
022300*    SUB�o�͏���                                                 *
022400*                                                                *
022500******************************************************************
022600 SUB-WRITE-PROC SECTION.
022700*G   SUB ���͏���
022800 SUB-WRITE-010.
022900     INITIALIZE O1-OUTFILE.
023000     MOVE WB-ROOT-KEY TO O1-ROOT-KEY.
023100     MOVE WB-SUB-KEY TO O1-SUB-KEY.
023200     WRITE O1-OUTFILE
023300     END-WRITE.
023400     INITIALIZE W1-OUTFILE.
023500 SUB-WRITE-999.
023600 EXIT.
023700                                                             SKIP3
023800******************************************************************
023900*                                                                *
024000*    SUB2�o�͏���                                                *
024100*                                                                *
024200******************************************************************
024300 SUB2-WRITE-PROC SECTION.
024400*G   SUB2 ���͏���
024500 SUB2-WRITE-010.
024600     INITIALIZE O1-OUTFILE.
024700     MOVE WC-ROOT-KEY TO W1-ROOT-KEY.
024800     MOVE WC-SUB-KEY  TO W1-SUB-KEY.
024900     MOVE WC-SUB2-KEY TO W1-SUB2-KEY.
025000     MOVE W1-OUTFILE TO O1-OUTFILE.
025100     WRITE O1-OUTFILE
025200     END-WRITE.
025300     INITIALIZE W1-OUTFILE.
025400 SUB2-WRITE-999.
025500 EXIT.
025600                                                             SKIP3
025700******************************************************************
025800*                                                                *
025900*    ��������                                                    *
026000*                                                                *
026100******************************************************************
026200 JUNBI-PROC                        SECTION.
026300*G   ��������
026400 JUNBI-010.
026500
026600     ACCEPT W2-START-TIME FROM TIME.
026700
026800 JUNBI-999.
026900 EXIT.
027000                                                             SKIP3
027100******************************************************************
027200*                                                                *
027300*    �I������                                                    *
027400*                                                                *
027500******************************************************************
027600 SYURYO-PROC                       SECTION.
027700*G   �I������
027800 SYURYO-010.
027900
028000     ACCEPT W2-END-TIME FROM TIME.
028100
028200     OPEN   OUTPUT FILE-OUT-STATUS.
028300
028400     COMPUTE W1-COUNTER = WA-COUNTER + WB-COUNTER + WC-COUNTER.
028500
028600     COMPUTE W2-STIME = ((W2-S_HH * 360000) + (W2-S_MM * 6000)
028700                                     + (W2-S_SS * 100) + W2-S_TT)
028800     COMPUTE W2-ETIME = ((W2-E_HH * 360000) + (W2-E_MM * 6000)
028900                                    + (W2-E_SS * 100) + W2-E_TT).
029000     COMPUTE W2-XTIME = W2-ETIME - W2-STIME.
029100
029200     DISPLAY "FILE-A COUNTER : " WA-COUNTER.
029300     DISPLAY "FILE-B COUNTER : " WB-COUNTER.
029400     DISPLAY "FILE-C COUNTER : " WC-COUNTER.
029500     DISPLAY "TOTAL  COUNTER : " W1-COUNTER.
029600
029700     DISPLAY "�J�n���ԁ@�F" W2-S_HH "��" W2-S_MM "��"
029800                            W2-S_SS "�b" W2-S_TT
029900     DISPLAY "�I�����ԁ@�F" W2-E_HH "��" W2-E_MM "��"
030000                            W2-E_SS "�b" W2-E_TT
030100     DISPLAY "�J�n�~���b�F" W2-STIME
030200     DISPLAY "�I���~���b�F" W2-ETIME
030300     DISPLAY "���s�~���b�F" W2-XTIME
030400
030500*    MOVE "�e�t�@�C���Ǎ��񐔁F" TO O_REC_COUNT-A-ST.
030600     MOVE WA-COUNTER TO O2-REC_COUNT-A.
030700     WRITE O2-COUNT-A.
030800*    MOVE "�q�t�@�C���Ǎ��񐔁F" TO O_REC_COUNT-B-ST.
030900     MOVE WB-COUNTER TO O2-REC_COUNT-B.
031000     WRITE O2-COUNT-B.
031100*    MOVE "���t�@�C���Ǎ��񐔁F" TO O_REC_COUNT-C-ST;
031200     MOVE WC-COUNTER TO O2-REC_COUNT-C.
031300     WRITE O2-COUNT-C.
031400*    MOVE "���v�Ǎ��񐔁F" TO O_FILE-OUT-REC-CNT-ST;
031500     MOVE W1-COUNTER TO O2-FILE-OUT-REC-CNT 
031600                     TO O2-FILE-OUT-REC-CNT.
031700     WRITE O2-COUNT-TOTAL.
031800
031900     MOVE W2-STIME TO O2-STIME.
032000     WRITE O2-XTIME.
032100     MOVE W2-ETIME TO O2-ETIME.
032200     WRITE O2-XTIME.
032300     MOVE W2-XTIME TO O2-XTIME.
032400     WRITE O2-XTIME.
032500
032600     CLOSE FILE-OUT-STATUS.
032700
032800 SYURYO-999.
032900     EXIT.

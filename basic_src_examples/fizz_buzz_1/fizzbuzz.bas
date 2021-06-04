'
' FIZZ BUZZ (1)
'
DIM I AS INTEGER
DIM C AS INTEGER
PRINT "LIMIT?"
INPUT C
C = MAX(1, MIN(100, C))
FOR I = 1 TO C STEP 1
    SELECT CASE I MOD 15
        CASE 0
            PRINT "FIZZBUZZ"
        CASE 3, 6, 9, 12
            PRINT "FIZZ"
        CASE 5, 10
            PRINT "BUZZ"
        CASE ELSE
            PRINT I
    END SELECT
NEXT I

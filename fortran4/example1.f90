C     --- PROGRAMA 1 -----
      read 6, i,k,j
   99 if (i .lt. j) goto 33
      goto 55
   33 i = j
      goto 99
   55 k = j + 1
      stop


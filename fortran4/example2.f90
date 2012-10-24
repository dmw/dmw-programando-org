C     --- PROGRAMA 2 -----
      read 6, i, k, j
      if (i .lt. j) goto 12345
77    k = j + 1
      goto 5555
12345 i = j
      if (i .lt. j) goto 12345
      goto 77
88    goto 88
5555  stop


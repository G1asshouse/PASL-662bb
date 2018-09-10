PROGRAM ListOptions
  IMPLICIT none

  LOGICAL :: pwants = .FALSE., qwants = .FALSE.
  INTEGER :: i, j, k, l, cnt = 0, p = 0, q = 0, turn = 1, possible = 0
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: comb1
  LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: comb2
  ALLOCATE(comb1(288,3))
  ALLOCATE(comb2(288,2))
  
  DO i=0,5
     p = p+1
     q = 0

     DO j=0,5
       q = q+1
       DO k=1,8
          cnt = cnt + 1
          comb1(cnt,1) = p
          comb1(cnt,2) = q
          comb1(cnt,3) = turn
          comb2(cnt,1) = pwants
          comb2(cnt,2) = qwants
          
          SELECT CASE (MOD(k,2))
            CASE (0)
              pwants = .NOT. pwants
            CASE (1)
              qwants = .NOT. qwants
           END SELECT

           IF (0 == MOD(k,4)) THEN
              IF (turn == 1) THEN
                 turn = 2
              ELSE
                 turn = 1
              END IF
           END IF
              
      END DO
    END DO
  END DO

  DO l=1,cnt
    PRINT "('(',I1,2I2,2L2')')", comb1(l,1),comb1(l,2),comb1(l,3),comb2(l,1),comb2(l,2)
 END DO
END PROGRAM ListOptions

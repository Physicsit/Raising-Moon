        implicit none
        integer row,col,amat,bmat,c,i,j,e,d,p,r,s,t,diff,add,k,a
        dimension amat(10,10),bmat(10,10),d(10,10),e(10,10)
        dimension add(10,10),diff(10,10),a(10,10)
        open(7,file='subtract')
        PRINT*,'         MATRIX ADDITION AND SUBTRACTION              '
        print*,'enter the number of rows and columns'
        write(7,*)'enter the number of rows and columns'
        read(5,*)row,col
        write(7,*)row,col
        print*,'enter the elements of matrix A'
        write(7,*)'enter the elements of matrix A'
        read(5,*)((amat(i,j),j=1,col),i=1,row)
        WRITE(7,*)((amat(i,j),j=1,col),i=1,row)
        print*,'matrix A='
        write(7,*)'matrix A='
        DO 1 i=1,row
        print*,(amat(i,j),j=1,col)
        write(7,*)(amat(i,j),j=1,col)
    1   continue
        print*,'enter the elements of matrix B'
        write(7,*)'enter the elements of matrix B'
        read(5,*)((bmat(i,j),j=1,col),i=1,row)
        write(7,*)((bmat(i,j),j=1,col),i=1,row)
        PRINT*,'matrix B='
        write(7,*)'matrix B='
        DO 2 i=1,row
        print*,(bmat(i,j),j=1,col)
        write(7,*)(bmat(i,j),j=1,col)
    2   continue
        do 3,i=1,row
        do 4,j=1,col
        add(i,J)=amat(i,j)+bmat(i,j)
        Diff(i,j)=amat(i,j)-bmat(i,j)
   4    continue
   3    continue
        print*,'sum of matrix='
        write(7,*)'sum of matrix'
        do 5,i=1,row
        print*,(add(i,j),j=1,col)
        write(7,*)(add(i,j),j=1,col)
   5    continue
        print*,'difference of matrix ='
        write(7,*)'difference of matrix='
        do 6,i=1,row
        print*,(diff(i,j),j=1,col)
        write(7,*)(diff(i,j),j=1,col)
   6    continue
        print*,'            MATRIX MULTIPLICATION         '
   12   print*,'enter the number of rows and columns of matrix c'
        write(7,*)'enter the number of rows and columns of matrix c'
        read(5,*)p,r
        write(7,*)p,r
        print*,'enter the number of rows and columns of matrix d'
        write(7,*)'enter the number of rows and columns of matrix d'
        read(5,*)s,t
        write(7,*)s,t
        if(r.NE.s) then
        write(7,*)'matrices cannot be multiplied'
        print*,'matrices cannot be multipied'
        print*,'keep columns of c=rows of d'
        write(7,*)'keep columns of C=rows of d'
        GOTO 12
        endif
        print*,'enter the elements of matrix c'
        write(7,*)'enter the elements of matrix c'
        read(5,*)((a(i,j),j=1,r),i=1,p)
        write(7,*)((a(i,j),j=1,r),i=1,p)
        print*,'matrix C='
        write(7,*)'matrix C='
        do 7,i=1,p
        print*,(a(i,j),j=1,r)
        write(7,*)(a(i,j),j=1,r)
  7     continue
        print*,'enter the elements of matrix d'
        write(7,*)'enter the elements of matrix d'
        read(5,*)((d(i,j),j=1,t),i=1,s)
        write(7,*)((d(i,j),j=1,t),i=1,s)
        print*,'matrix D='
        write(7,*)'matrix D='
        do 8,i=1,s
        print*,(d(i,j),j=1,t)
        write(7,*)(d(i,j),j=1,t)
  8     continue
        do 9,i=1,p
        do 10,j=1,t
        e(i,j)=0
        do 11,k=1,r
        e(i,j)=e(i,j)+a(i,k)*d(k,j)
   11   continue
   10   continue
   9    continue
        print*,'product of matrix='
        write(7,*)'product of matrix='
        do i=1,p
        print*,(e(i,j),j=1,t)
        write(7,*)(e(i,j),j=1,t)
        enddo
        close(7)
        stop
        end


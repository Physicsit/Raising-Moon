        implicit none
        real a,b,x,error,f,n
        Integer count
        error=0.001
        open(3,file='Bisection')
        print*,'the equation is in trhe form,F(x)=2x**3-3x-6'
        write(3,*),'the equation is in the form,F(x)=2x**3-3x-6'
  20    Print*,'the value of a is'
        read*,a
        write(3,*),'the value of a is'
        print*,'the value of b is'
        read*,b
        write(3,*)'the value of a is',a
        print*,'the value of b is'
        read*,b
        write(3,*),'---------------------------------------------------'
        write(3,*),'a               b                   x    f(x)'
    !   bisection
        count=0
   30   if(f(a)*f(b).lt.0)then
        x=(a+b)/2
        else
        print*,'try with another value of a and b'
        write(3,*),'try with another value of a and b'
        goto 20
        end if
        count=count+1
        if(f(A)*f(x).lt.0)then
        b=x
        else
        a=x
        endif
        Print*,'x=',x
        write(3,40)a,b,x,f(x)
   40   format(f8.5,5x,f8.5,5x,f8.6,5x,f12.9)
        if(abs(b-a).gt.0)then
        goto 30
        write(*,*)'the number of iteration performed is',count
        write(3,*),'the number of iteration performed is',count
        print*,'the root is',x
        write(3,*),'the root is',x
        close(3)
        stop
        end

      program approximate
      real X(200), Y(200), G(200), P(300), C(300)
      open (11, FILE='Noob')
      open (10, FILE='Pro')
      G=1.
      k=100
      kk=30
      a=-10.
      b=10.
      f=0.
      h=(b-a)/k
      hh=(b-a)/kk
      do 1, i = 1, k
      X(i)=a+(i-1)*h
      Y(i)= floor(X(i))
      write (11, *) X(i), Y(i)
1     continue
      close (11)

      do 5 m=1, kk
      P(m)=a+(m-1)*hh
      t=P(m)
      f=0.
      do 2, l=1, k
      
      do 3, j=1, k
      if (j .ne. l) then
      G(l)=G(l)*(t-X(j))/(X(l)-X(j))
      endif
3     continue

      f=f+Y(l)*G(l)
2     continue
      write (10, *) P(m), f
5     continue
      close (10)
      end

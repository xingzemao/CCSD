program matMulProduct
implicit none
double precision,dimension(10,10)::a,b,c,d,e
integer::i,j,k
integer::n
n=10
do i=1,n
  do j =1,n
     a(i,j)=(i+j)*1.0d0
     e(i,j)=0.0d0
  end do
end do

do i=1,n
  do j=1,n
    b(i,j)=1.0d0
  end do
end do

do i=1,n
  do j=1,n
    c(i,j)=0
    do k=1,n
       c(i,j)=c(i,j)+a(i,k)*b(k,j)
    end do
  end do
end do

d=matmul(a,b)
do i=1,n
   write(*,*) (e(i,j),j=1,n)
end do

call dgemm('N','N',n,n,n,1.0d0,a,n,b,n,1.0d0,e,n)

!do i=1,n
!   write(*,*) ( a(i,j),j=1,n)
!end do
do i=1,n
   write(*,*) ( c(i,j),j=1,n)
end do
do i=1,n
   write(*,*) ( e(i,j)-c(i,j),j=1,n)
end do


end program matMulProduct

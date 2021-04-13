 program obldrv
    use param
    use oblate_swf

    implicit none

    integer   i, im, j, mmin, minc, mnum, m, l, lnum, ioprad, iopang, &
              iopnorm, ioparg, narg, kind, kindd, kindq
    real(knd) c, x, x1, arg, arg1, darg, pi, api
    character chr

    real(knd), dimension(:), allocatable ::    eta, r1c, r1dc, r2c, r2dc
    integer, dimension(:), allocatable ::      ir1e, ir1de, ir2e, ir2de, naccr
    real(knd), dimension (:,:), allocatable :: s1c, s1dc
    integer, dimension(:,:), allocatable ::    is1e, is1de, naccs, naccds

    kindd =  8
    kindq = 16

!
!  open input files
    open(1, file='oblfcn.dat')
    open(20 ,file='fort.20')
    open(30, file='fort.30')
    open(40 ,file='fort.40')
    open(50, file='fort.50')
    open(60 ,file='fort.60')

!
!  read input data
        read(1,*) mmin,minc,mnum,lnum
        read(1,*) ioprad,iopang,iopnorm
        read(1,*) c,x
        if(iopang.ne.0) read(1,*) ioparg,arg1,darg,narg

    allocate (eta(narg), r1c(lnum), r1dc(lnum), r2c(lnum), r2dc(lnum))
    allocate (ir1e(lnum), ir1de(lnum), ir2e(lnum), ir2de(lnum), naccr(lnum))
    allocate (s1c(lnum, narg), s1dc(lnum, narg))
    allocate (is1e(lnum, narg), is1de(lnum, narg), naccs(lnum, narg), naccds(lnum, narg))

    if (iopang /= 0) then
        do j = 1, narg
            eta(j) = arg1 + (j - 1) * darg
        end do
    end if

    do im = 1, mnum
        m = mmin + (im - 1) * minc

            call oblfcn(c, m, lnum, ioprad, x, iopang, iopnorm, narg, eta, &
                        r1c, ir1e, r1dc, ir1de, r2c, &
                        ir2e, r2dc, ir2de, naccr, &
                        s1c, is1e, s1dc, is1de, naccs, naccds)

        if (ioprad /= 0) then

            if(knd == kindd) write(20, 10) x, c, m
            if(knd == kindq) write(20, 20) x, c, m
10          format(1x,e23.14,e23.14,i5)
20          format(1x,e39.30,e39.30,i5)

            do i = 1, lnum
                l = m + i - 1
                chr = 'w'
                if (naccr(i) < 0) chr = 'e'
                if(ioprad == 2) write(20, 30) l, r1c(i), ir1e(i), r1dc(i), ir1de(i), r2c(i), ir2e(i), r2dc(i), ir2de(i), abs(naccr(i)), chr
                if(ioprad == 1) write(20, 40) l, r1c(i), ir1e(i), r1dc(i), ir1de(i)
30              format(1x,i5,2x,4(f17.14,i6,2x),i2, a)
40              format(1x,i5,2x,2(f17.14,i6,2x),i2)
            end do

        end if

        if (iopang /= 0) then

            if(knd == kindd) write(30, 50) c, m
            if(knd == kindq) write(30, 60) c, m
50          format(1x,e23.14,i5)
60          format(1x,e39.30,i5)

            do i = 1, lnum
                l = m + i - 1

                write(30, 70) l
70              format(1x,i6)

                do j = 1, narg
                    arg = arg1 + (j - 1) * darg
                    write(30, 90) arg, s1c(i, j), is1e(i, j), s1dc(i, j), is1de(i, j), naccs(i, j), naccds(i, j)
80                  format(1x,f19.14,2x,f17.14,2x,i5,2x,f17.14,2x,i5,2x,i2, ', ', i2)
90                  format(1x,f17.14,2x,f17.14,2x,i5,2x,f17.14,2x,i5,2x,i2,', ',i2)
                end do
            end do
        end if
    end do

    deallocate (is1e, is1de, naccs, naccds)
    deallocate (s1c, s1dc)
    deallocate (ir1e, ir1de, ir2e, ir2de, naccr)
    deallocate (eta, r1c, r1dc, r2c, r2dc)
    close(60)
    close(50)
    close(40)
    close(30)
    close(20)
    close(1)

end program obldrv

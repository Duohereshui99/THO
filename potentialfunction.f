        module potentialfunction
            implicit none
            contains
!Gaussian 势,v0深（使用时记得带负号）,
!r0中心位置通常为0,a描述宽度。输出r处的势。
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
            function gausspot(r,v0,r0,a)
                implicit none
                 real*8 r,v0,r0,gausspot,a
                   if (a.gt.1e-6) then
                     gausspot=V0*exp(-(r-r0)**2/a**2)
                       else
                         write(*,*)'a too small in gausspot!'
                         stop
                   endif
                   return
                end function
        end module

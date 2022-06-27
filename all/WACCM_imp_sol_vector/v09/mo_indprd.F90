!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2






      module mo_indprd

          USE shr_kind_mod, ONLY: rkind_comp
          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE chem_mods, ONLY: clscnt4, gas_pcnst, rxntot, extcnt
          PRIVATE
          PUBLIC indprd

      contains

      subroutine indprd( class, prod, y, extfrc, rxt, chnkpnts )


      implicit none

!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: class
      integer, intent(in) :: chnkpnts
      real(rkind_comp), intent(in) :: y(chnkpnts,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(chnkpnts,rxntot)
      real(rkind_comp), intent(in) :: extfrc(chnkpnts,extcnt)
      real(rkind_comp), intent(inout) :: prod(chnkpnts,clscnt4)
      integer :: i
      !$acc declare present(y,rxt,extfrc,prod)

!--------------------------------------------------------------------
! ... "independent" production for Explicit species
!--------------------------------------------------------------------
      if( class == 1 ) then
       !$acc parallel loop gang vector default(present)
       do i=1,chnkpnts
         prod(i,1) = 0._rkind_comp

         prod(i,2) =.080_rkind_comp*rxt(i,311)*y(i,59)*y(i,1)

         prod(i,3) =rxt(i,184)*y(i,10)*y(i,8)

         prod(i,4) = 0._rkind_comp

         prod(i,5) = 0._rkind_comp

         prod(i,6) = 0._rkind_comp

         prod(i,7) = 0._rkind_comp

         prod(i,8) = 0._rkind_comp

         prod(i,9) = 0._rkind_comp

         prod(i,10) = 0._rkind_comp

         prod(i,11) = 0._rkind_comp

         prod(i,12) = 0._rkind_comp

         prod(i,13) = 0._rkind_comp

         prod(i,14) = 0._rkind_comp

         prod(i,15) = 0._rkind_comp

         prod(i,16) = 0._rkind_comp

         prod(i,17) = 0._rkind_comp

         prod(i,18) = 0._rkind_comp

         prod(i,19) = 0._rkind_comp

         prod(i,20) = 0._rkind_comp

         prod(i,21) = 0._rkind_comp

         prod(i,22) = (rxt(i,264)*y(i,20) +rxt(i,265)*y(i,20) +rxt(i,276)*y(i,136) + &
                 rxt(i,291)*y(i,49) +.500_rkind_comp*rxt(i,304)*y(i,54) + &
                 .800_rkind_comp*rxt(i,305)*y(i,52) +rxt(i,306)*y(i,53) + &
                 .500_rkind_comp*rxt(i,355)*y(i,81))*y(i,23) + (rxt(i,299)*y(i,9) + &
                 .900_rkind_comp*rxt(i,302)*y(i,16) +2.000_rkind_comp*rxt(i,303)*y(i,48) + &
                 2.000_rkind_comp*rxt(i,351)*y(i,76) +rxt(i,379)*y(i,91))*y(i,48) &
                  + (rxt(i,350)*y(i,16) +2.000_rkind_comp*rxt(i,352)*y(i,76))*y(i,76) &
                  +rxt(i,63)*y(i,54) +.400_rkind_comp*rxt(i,64)*y(i,58)

         prod(i,23) = 0._rkind_comp

         prod(i,24) = 0._rkind_comp

         prod(i,25) = 0._rkind_comp

         prod(i,26) = 0._rkind_comp

         prod(i,27) = 0._rkind_comp

         prod(i,28) = 0._rkind_comp

         prod(i,29) = 0._rkind_comp

         prod(i,30) = 0._rkind_comp

         prod(i,31) = + extfrc(i,15)

         prod(i,32) = 0._rkind_comp

         prod(i,33) = 0._rkind_comp

         prod(i,34) = 0._rkind_comp

         prod(i,35) = 0._rkind_comp

         prod(i,36) = 0._rkind_comp

         prod(i,37) = 0._rkind_comp
       enddo
!--------------------------------------------------------------------
! ... "independent" production for Implicit species
!--------------------------------------------------------------------
      else if( class == 4 ) then
       !$acc parallel loop gang vector default(present)
       do i=1,chnkpnts
         prod(i,139) = 0._rkind_comp

         prod(i,146) = (rxt(i,58) +rxt(i,114))*y(i,126) +.180_rkind_comp*rxt(i,60)*y(i,15)

         prod(i,143) =rxt(i,5)*y(i,7)

         prod(i,129) = 0._rkind_comp

         prod(i,43) = 0._rkind_comp

         prod(i,42) = 0._rkind_comp

         prod(i,120) =1.440_rkind_comp*rxt(i,60)*y(i,15)

         prod(i,115) = (rxt(i,58) +rxt(i,114))*y(i,126) +.380_rkind_comp*rxt(i,60)*y(i,15) &
                  + extfrc(i,3)

         prod(i,105) = (rxt(i,98) +.800_rkind_comp*rxt(i,101) +rxt(i,110) +.800_rkind_comp*rxt(i,113)) &
                  + extfrc(i,12)

         prod(i,134) = + extfrc(i,1)

         prod(i,144) = + extfrc(i,2)

         prod(i,138) =.330_rkind_comp*rxt(i,60)*y(i,15) + extfrc(i,14)

         prod(i,135) = 0._rkind_comp

         prod(i,131) = 0._rkind_comp

         prod(i,73) = 0._rkind_comp

         prod(i,55) = 0._rkind_comp

         prod(i,136) =rxt(i,59)*y(i,15) +rxt(i,37)*y(i,108) +rxt(i,48)*y(i,109)

         prod(i,67) = 0._rkind_comp

         prod(i,47) = 0._rkind_comp

         prod(i,34) = 0._rkind_comp

         prod(i,140) =.180_rkind_comp*rxt(i,60)*y(i,15)

         prod(i,141) = (rxt(i,59) +.330_rkind_comp*rxt(i,60))*y(i,15)

         prod(i,142) = 0._rkind_comp

         prod(i,89) = 0._rkind_comp

         prod(i,130) =.050_rkind_comp*rxt(i,60)*y(i,15)

         prod(i,145) =rxt(i,37)*y(i,108) +2.000_rkind_comp*rxt(i,40)*y(i,110) &
                  +2.000_rkind_comp*rxt(i,41)*y(i,111) +2.000_rkind_comp*rxt(i,42)*y(i,112) +rxt(i,45) &
                 *y(i,113) +4.000_rkind_comp*rxt(i,38)*y(i,114) +3.000_rkind_comp*rxt(i,39)*y(i,115) &
                  +rxt(i,50)*y(i,117) +rxt(i,46)*y(i,118) +rxt(i,47)*y(i,119) &
                  +2.000_rkind_comp*rxt(i,43)*y(i,120) +rxt(i,44)*y(i,121)

         prod(i,44) = 0._rkind_comp

         prod(i,132) = 0._rkind_comp

         prod(i,37) = 0._rkind_comp

         prod(i,28) = 0._rkind_comp

         prod(i,137) = 0._rkind_comp

         prod(i,107) = 0._rkind_comp

         prod(i,113) = 0._rkind_comp

         prod(i,50) = 0._rkind_comp

         prod(i,133) =rxt(i,48)*y(i,109) +rxt(i,49)*y(i,116) +rxt(i,50)*y(i,117) &
                  +2.000_rkind_comp*rxt(i,53)*y(i,122) +2.000_rkind_comp*rxt(i,54)*y(i,123) &
                  +3.000_rkind_comp*rxt(i,51)*y(i,124) +2.000_rkind_comp*rxt(i,52)*y(i,125)

         prod(i,128) = 0._rkind_comp

         prod(i,104) = 0._rkind_comp

         prod(i,96) = 0._rkind_comp

         prod(i,80) = 0._rkind_comp

         prod(i,92) = (rxt(i,94) +rxt(i,106)) + extfrc(i,10)

         prod(i,98) = + extfrc(i,8)

         prod(i,72) = (rxt(i,98) +rxt(i,99) +rxt(i,110) +rxt(i,111)) + extfrc(i,9)

         prod(i,88) = + extfrc(i,7)

         prod(i,99) = 0._rkind_comp

         prod(i,76) = (rxt(i,99) +1.200_rkind_comp*rxt(i,101) +rxt(i,111) + &
                 1.200_rkind_comp*rxt(i,113)) + extfrc(i,11)

         prod(i,100) = (rxt(i,94) +rxt(i,98) +rxt(i,99) +rxt(i,106) +rxt(i,110) + &
                 rxt(i,111)) + extfrc(i,13)

         prod(i,114) = 0._rkind_comp

         prod(i,109) = 0._rkind_comp

         prod(i,103) = 0._rkind_comp

         prod(i,116) = 0._rkind_comp

         prod(i,87) = 0._rkind_comp

         prod(i,82) = 0._rkind_comp

         prod(i,127) = 0._rkind_comp

         prod(i,75) = 0._rkind_comp

         prod(i,74) = 0._rkind_comp

         prod(i,61) = 0._rkind_comp

         prod(i,53) = 0._rkind_comp

         prod(i,77) = 0._rkind_comp

         prod(i,29) = 0._rkind_comp

         prod(i,81) = 0._rkind_comp

         prod(i,30) = 0._rkind_comp

         prod(i,56) = 0._rkind_comp

         prod(i,93) = 0._rkind_comp

         prod(i,90) = 0._rkind_comp

         prod(i,68) = 0._rkind_comp

         prod(i,91) = 0._rkind_comp

         prod(i,57) = 0._rkind_comp

         prod(i,38) = 0._rkind_comp

         prod(i,39) = 0._rkind_comp

         prod(i,84) = 0._rkind_comp

         prod(i,62) = 0._rkind_comp

         prod(i,45) = 0._rkind_comp

         prod(i,112) = 0._rkind_comp

         prod(i,70) = 0._rkind_comp

         prod(i,85) = 0._rkind_comp

         prod(i,95) = 0._rkind_comp

         prod(i,31) = 0._rkind_comp

         prod(i,63) = 0._rkind_comp

         prod(i,1) = 0._rkind_comp

         prod(i,32) = 0._rkind_comp

         prod(i,71) = 0._rkind_comp

         prod(i,2) = 0._rkind_comp

         prod(i,123) = 0._rkind_comp

         prod(i,125) = 0._rkind_comp

         prod(i,119) = 0._rkind_comp

         prod(i,124) = 0._rkind_comp

         prod(i,58) = 0._rkind_comp

         prod(i,126) = 0._rkind_comp

         prod(i,106) = 0._rkind_comp

         prod(i,59) = 0._rkind_comp

         prod(i,86) = 0._rkind_comp

         prod(i,35) = 0._rkind_comp

         prod(i,108) = 0._rkind_comp

         prod(i,64) = 0._rkind_comp

         prod(i,94) = 0._rkind_comp

         prod(i,65) = 0._rkind_comp

         prod(i,79) = 0._rkind_comp

         prod(i,51) = 0._rkind_comp

         prod(i,110) = 0._rkind_comp

         prod(i,118) = 0._rkind_comp

         prod(i,97) = 0._rkind_comp

         prod(i,69) = 0._rkind_comp

         prod(i,40) = 0._rkind_comp

         prod(i,60) = 0._rkind_comp

         prod(i,117) = 0._rkind_comp

         prod(i,121) = 0._rkind_comp

         prod(i,102) = 0._rkind_comp

         prod(i,111) = 0._rkind_comp

         prod(i,122) = 0._rkind_comp

         prod(i,52) = 0._rkind_comp

         prod(i,83) = 0._rkind_comp

         prod(i,54) = 0._rkind_comp

         prod(i,78) = 0._rkind_comp

         prod(i,66) = 0._rkind_comp

         prod(i,41) =rxt(i,41)*y(i,111) +rxt(i,42)*y(i,112) +rxt(i,45)*y(i,113) &
                  +rxt(i,49)*y(i,116) +rxt(i,50)*y(i,117) +rxt(i,47)*y(i,119) &
                  +2.000_rkind_comp*rxt(i,43)*y(i,120) +2.000_rkind_comp*rxt(i,44)*y(i,121) +rxt(i,53) &
                 *y(i,122) +2.000_rkind_comp*rxt(i,54)*y(i,123)

         prod(i,46) =rxt(i,40)*y(i,110) +rxt(i,42)*y(i,112) +rxt(i,46)*y(i,118)

         prod(i,48) = 0._rkind_comp

         prod(i,101) =rxt(i,49)*y(i,116) +rxt(i,44)*y(i,121)

         prod(i,36) = + extfrc(i,4)

         prod(i,49) = 0._rkind_comp

         prod(i,3) = + extfrc(i,5)

         prod(i,33) = 0._rkind_comp

         prod(i,4) = 0._rkind_comp

         prod(i,5) = 0._rkind_comp

         prod(i,6) = 0._rkind_comp

         prod(i,7) = 0._rkind_comp

         prod(i,8) = 0._rkind_comp

         prod(i,9) = 0._rkind_comp

         prod(i,10) = 0._rkind_comp

         prod(i,11) = 0._rkind_comp

         prod(i,12) = 0._rkind_comp

         prod(i,13) = 0._rkind_comp

         prod(i,14) = 0._rkind_comp

         prod(i,15) = 0._rkind_comp

         prod(i,16) = + extfrc(i,6)

         prod(i,17) = 0._rkind_comp

         prod(i,18) = 0._rkind_comp

         prod(i,19) = 0._rkind_comp

         prod(i,20) = 0._rkind_comp

         prod(i,21) = 0._rkind_comp

         prod(i,22) = 0._rkind_comp

         prod(i,23) = 0._rkind_comp

         prod(i,24) = 0._rkind_comp

         prod(i,25) = 0._rkind_comp

         prod(i,26) = 0._rkind_comp

         prod(i,27) = 0._rkind_comp
       enddo

      end if

      end subroutine indprd

      end module mo_indprd

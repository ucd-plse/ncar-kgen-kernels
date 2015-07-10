
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_lw_rad.f90
! Generated at: 2015-07-06 23:28:44
! KGEN version: 0.4.13



    MODULE rrtmg_lw_rad
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !  --------------------------------------------------------------------------
        ! |                                                                          |
        ! |  Copyright 2002-2007, Atmospheric & Environmental Research, Inc. (AER).  |
        ! |  This software may be used, copied, or redistributed as long as it is    |
        ! |  not sold and this copyright notice is reproduced on each copy made.     |
        ! |  This model is provided as is without any express or implied warranties. |
        ! |                       (http://www.rtweb.aer.com/)                        |
        ! |                                                                          |
        !  --------------------------------------------------------------------------
        !
        ! ****************************************************************************
        ! *                                                                          *
        ! *                              RRTMG_LW                                    *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                   a rapid radiative transfer model                       *
        ! *                       for the longwave region                            *
        ! *             for application to general circulation models                *
        ! *                                                                          *
        ! *                                                                          *
        ! *            Atmospheric and Environmental Research, Inc.                  *
        ! *                        131 Hartwell Avenue                               *
        ! *                        Lexington, MA 02421                               *
        ! *                                                                          *
        ! *                                                                          *
        ! *                           Eli J. Mlawer                                  *
        ! *                        Jennifer S. Delamere                              *
        ! *                         Michael J. Iacono                                *
        ! *                         Shepard A. Clough                                *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                       email:  miacono@aer.com                            *
        ! *                       email:  emlawer@aer.com                            *
        ! *                       email:  jdelamer@aer.com                           *
        ! *                                                                          *
        ! *        The authors wish to acknowledge the contributions of the          *
        ! *        following people:  Steven J. Taubman, Karen Cady-Pereira,         *
        ! *        Patrick D. Brown, Ronald E. Farren, Luke Chen, Robert Bergstrom.  *
        ! *                                                                          *
        ! ****************************************************************************
        ! -------- Modules --------
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        !      use parkind, only : jpim, jprb
        USE rrtmg_lw_cldprmc, ONLY: cldprmc
        ! Move call to rrtmg_lw_ini and following use association to
        ! GCM initialization area
        !      use rrtmg_lw_init, only: rrtmg_lw_ini
        USE rrtmg_lw_rtrnmc, ONLY: rtrnmc
        USE rrtmg_lw_setcoef, ONLY: setcoef
        USE rrtmg_lw_taumol, ONLY: taumol
        IMPLICIT NONE
        ! public interfaces/functions/subroutines
        PUBLIC rrtmg_lw, inatm
        !------------------------------------------------------------------
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !------------------------------------------------------------------
        !------------------------------------------------------------------
        ! Public subroutines
        !------------------------------------------------------------------

        SUBROUTINE rrtmg_lw(lchnk, ncol, nlay, icld, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, &
        cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, &
        relqmcl, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc, uflxs, dflxs)
            ! -------- Description --------
            ! This program is the driver subroutine for RRTMG_LW, the AER LW radiation
            ! model for application to GCMs, that has been adapted from RRTM_LW for
            ! improved efficiency.
            !
            ! NOTE: The call to RRTMG_LW_INI should be moved to the GCM initialization
            !  area, since this has to be called only once.
            !
            ! This routine:
            !    a) calls INATM to read in the atmospheric profile from GCM;
            !       all layering in RRTMG is ordered from surface to toa.
            !    b) calls CLDPRMC to set cloud optical depth for McICA based
            !       on input cloud properties
            !    c) calls SETCOEF to calculate various quantities needed for
            !       the radiative transfer algorithm
            !    d) calls TAUMOL to calculate gaseous optical depths for each
            !       of the 16 spectral bands
            !    e) calls RTRNMC (for both clear and cloudy profiles) to perform the
            !       radiative transfer calculation using McICA, the Monte-Carlo
            !       Independent Column Approximation, to represent sub-grid scale
            !       cloud variability
            !    f) passes the necessary fluxes and cooling rates back to GCM
            !
            ! Two modes of operation are possible:
            !     The mode is chosen by using either rrtmg_lw.nomcica.f90 (to not use
            !     McICA) or rrtmg_lw.f90 (to use McICA) to interface with a GCM.
            !
            !    1) Standard, single forward model calculation (imca = 0)
            !    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al.,
            !       JC, 2003) method is applied to the forward model calculation (imca = 1)
            !
            ! This call to RRTMG_LW must be preceeded by a call to the module
            !     mcica_subcol_gen_lw.f90 to run the McICA sub-column cloud generator,
            !     which will provide the cloud physical or cloud optical properties
            !     on the RRTMG quadrature point (ngpt) dimension.
            !
            ! Two methods of cloud property input are possible:
            !     Cloud properties can be input in one of two ways (controlled by input
            !     flags inflglw, iceflglw, and liqflglw; see text file rrtmg_lw_instructions
            !     and subroutine rrtmg_lw_cldprop.f90 for further details):
            !
            !    1) Input cloud fraction and cloud optical depth directly (inflglw = 0)
            !    2) Input cloud fraction and cloud physical properties (inflglw = 1 or 2);
            !       cloud optical properties are calculated by cldprop or cldprmc based
            !       on input settings of iceflglw and liqflglw
            !
            ! One method of aerosol property input is possible:
            !     Aerosol properties can be input in only one way (controlled by input
            !     flag iaer, see text file rrtmg_lw_instructions for further details):
            !
            !    1) Input aerosol optical depth directly by layer and spectral band (iaer=10);
            !       band average optical depth at the mid-point of each spectral band.
            !       RRTMG_LW currently treats only aerosol absorption;
            !       scattering capability is not presently available.
            !
            !
            ! ------- Modifications -------
            !
            ! This version of RRTMG_LW has been modified from RRTM_LW to use a reduced
            ! set of g-points for application to GCMs.
            !
            !-- Original version (derived from RRTM_LW), reduction of g-points, other
            !   revisions for use with GCMs.
            !     1999: M. J. Iacono, AER, Inc.
            !-- Adapted for use with NCAR/CAM.
            !     May 2004: M. J. Iacono, AER, Inc.
            !-- Revised to add McICA capability.
            !     Nov 2005: M. J. Iacono, AER, Inc.
            !-- Conversion to F90 formatting for consistency with rrtmg_sw.
            !     Feb 2007: M. J. Iacono, AER, Inc.
            !-- Modifications to formatting to use assumed-shape arrays.
            !     Aug 2007: M. J. Iacono, AER, Inc.
            !-- Modified to add longwave aerosol absorption.
            !     Apr 2008: M. J. Iacono, AER, Inc.
            ! --------- Modules ----------
            USE parrrtm, ONLY: mxmol
            USE parrrtm, ONLY: maxxsec
            USE parrrtm, ONLY: nbndlw
            USE parrrtm, ONLY: ngptlw
            USE rrlw_con, ONLY: oneminus
            USE rrlw_con, ONLY: pi
            USE rrlw_con, ONLY: fluxfac
            USE rrlw_wvn, ONLY: ngb
            ! ------- Declarations -------
            ! ----- Input -----
            INTEGER, intent(in) :: lchnk ! chunk identifier
            INTEGER, intent(in) :: ncol ! Number of horizontal columns
            INTEGER, intent(in) :: nlay ! Number of model layers
            INTEGER, intent(inout) :: icld ! Cloud overlap method
            !    0: Clear only
            !    1: Random
            !    2: Maximum/random
            !    3: Maximum
            REAL(KIND=r8), intent(in) :: play(:,:) ! Layer pressures (hPa, mb)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: plev(:,:) ! Interface pressures (hPa, mb)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(in) :: tlay(:,:) ! Layer temperatures (K)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: tlev(:,:) ! Interface temperatures (K)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(in) :: tsfc(:) ! Surface temperature (K)
            !    Dimensions: (ncol)
            REAL(KIND=r8), intent(in) :: h2ovmr(:,:) ! H2O volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: o3vmr(:,:) ! O3 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: co2vmr(:,:) ! CO2 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: ch4vmr(:,:) ! Methane volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: o2vmr(:,:) ! O2 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: n2ovmr(:,:) ! Nitrous oxide volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: cfc11vmr(:,:) ! CFC11 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: cfc12vmr(:,:) ! CFC12 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: cfc22vmr(:,:) ! CFC22 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: ccl4vmr(:,:) ! CCL4 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: emis(:,:) ! Surface emissivity
            !    Dimensions: (ncol,nbndlw)
            INTEGER, intent(in) :: inflglw ! Flag for cloud optical properties
            INTEGER, intent(in) :: iceflglw ! Flag for ice particle specification
            INTEGER, intent(in) :: liqflglw ! Flag for liquid droplet specification
            REAL(KIND=r8), intent(in) :: cldfmcl(:,:,:) ! Cloud fraction
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: ciwpmcl(:,:,:) ! Cloud ice water path (g/m2)
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: clwpmcl(:,:,:) ! Cloud liquid water path (g/m2)
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: reicmcl(:,:) ! Cloud ice effective radius (microns)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: relqmcl(:,:) ! Cloud water drop effective radius (microns)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: taucmcl(:,:,:) ! Cloud optical depth
            !    Dimensions: (ngptlw,ncol,nlay)
            !      real(kind=r8), intent(in) :: ssacmcl(:,:,:)      ! Cloud single scattering albedo
            !    Dimensions: (ngptlw,ncol,nlay)
            !   for future expansion
            !   lw scattering not yet available
            !      real(kind=r8), intent(in) :: asmcmcl(:,:,:)      ! Cloud asymmetry parameter
            !    Dimensions: (ngptlw,ncol,nlay)
            !   for future expansion
            !   lw scattering not yet available
            REAL(KIND=r8), intent(in) :: tauaer(:,:,:) ! aerosol optical depth
            !   at mid-point of LW spectral bands
            !    Dimensions: (ncol,nlay,nbndlw)
            !      real(kind=r8), intent(in) :: ssaaer(:,:,:)       ! aerosol single scattering albedo
            !    Dimensions: (ncol,nlay,nbndlw)
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            !      real(kind=r8), intent(in) :: asmaer(:,:,:)       ! aerosol asymmetry parameter
            !    Dimensions: (ncol,nlay,nbndlw)
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            ! ----- Output -----
            REAL(KIND=r8), intent(out) :: uflx(:,:) ! Total sky longwave upward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(out) :: dflx(:,:) ! Total sky longwave downward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(out) :: hr(:,:) ! Total sky longwave radiative heating rate (K/d)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(out) :: uflxc(:,:) ! Clear sky longwave upward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(out) :: dflxc(:,:) ! Clear sky longwave downward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(out) :: hrc(:,:) ! Clear sky longwave radiative heating rate (K/d)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(out) :: uflxs(:,:,:) ! Total sky longwave upward flux spectral (W/m2)
            !    Dimensions: (nbndlw,ncol,nlay+1)
            REAL(KIND=r8), intent(out) :: dflxs(:,:,:) ! Total sky longwave downward flux spectral (W/m2)
            !    Dimensions: (nbndlw,ncol,nlay+1)
            ! ----- Local -----
            ! Control
            INTEGER :: istart ! beginning band of calculation
            INTEGER :: iend ! ending band of calculation
            INTEGER :: iout ! output option flag (inactive)
            INTEGER :: iaer ! aerosol option flag
            INTEGER :: iplon ! column loop index
            ! flag for mcica [0=off, 1=on]
            INTEGER :: ims ! value for changing mcica permute seed
            INTEGER :: k ! layer loop index
            INTEGER :: ig ! g-point loop index
            ! Atmosphere
            REAL(KIND=r8) :: pavel(ncol,nlay) ! layer pressures (mb)
            REAL(KIND=r8) :: tavel(ncol,nlay) ! layer temperatures (K)
            REAL(KIND=r8) :: pz(ncol,0:nlay) ! level (interface) pressures (hPa, mb)
            REAL(KIND=r8) :: tz(ncol,0:nlay) ! level (interface) temperatures (K)
            REAL(KIND=r8) :: tbound(ncol) ! surface temperature (K)
            REAL(KIND=r8) :: coldry(ncol,nlay) ! dry air column density (mol/cm2)
            REAL(KIND=r8) :: wbrodl(ncol,nlay) ! broadening gas column density (mol/cm2)
            REAL(KIND=r8) :: wkl(ncol,mxmol,nlay) ! molecular amounts (mol/cm-2)
            REAL(KIND=r8) :: wx(ncol,maxxsec,nlay) ! cross-section amounts (mol/cm-2)
            REAL(KIND=r8) :: pwvcm(ncol) ! precipitable water vapor (cm)
            REAL(KIND=r8) :: semiss(ncol,nbndlw) ! lw surface emissivity
            REAL(KIND=r8) :: fracs(ncol,nlay,ngptlw) !
            REAL(KIND=r8) :: taug(ncol,nlay,ngptlw) ! gaseous optical depths
            REAL(KIND=r8) :: taut(ncol,nlay,ngptlw) ! gaseous + aerosol optical depths
            REAL(KIND=r8) :: taua(ncol,nlay,nbndlw) ! aerosol optical depth
            !      real(kind=r8) :: ssaa(nlay,nbndlw)        ! aerosol single scattering albedo
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            !      real(kind=r8) :: asma(nlay+1,nbndlw)      ! aerosol asymmetry parameter
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            ! Atmosphere - setcoef
            INTEGER :: laytrop(ncol) ! tropopause layer index
            INTEGER :: jp(ncol,nlay) ! lookup table index
            INTEGER :: jt(ncol,nlay) ! lookup table index
            INTEGER :: jt1(ncol,nlay) ! lookup table index
            REAL(KIND=r8) :: planklay(ncol,nlay,nbndlw) !
            REAL(KIND=r8) :: planklev(ncol,0:nlay,nbndlw) !
            REAL(KIND=r8) :: plankbnd(ncol,nbndlw) !
            REAL(KIND=r8) :: colh2o(ncol,nlay) ! column amount (h2o)
            REAL(KIND=r8) :: colco2(ncol,nlay) ! column amount (co2)
            REAL(KIND=r8) :: colo3(ncol,nlay) ! column amount (o3)
            REAL(KIND=r8) :: coln2o(ncol,nlay) ! column amount (n2o)
            REAL(KIND=r8) :: colco(ncol,nlay) ! column amount (co)
            REAL(KIND=r8) :: colch4(ncol,nlay) ! column amount (ch4)
            REAL(KIND=r8) :: colo2(ncol,nlay) ! column amount (o2)
            REAL(KIND=r8) :: colbrd(ncol,nlay) ! column amount (broadening gases)
            INTEGER :: indself(ncol,nlay)
            INTEGER :: indfor(ncol,nlay)
            REAL(KIND=r8) :: selffac(ncol,nlay)
            REAL(KIND=r8) :: selffrac(ncol,nlay)
            REAL(KIND=r8) :: forfac(ncol,nlay)
            REAL(KIND=r8) :: forfrac(ncol,nlay)
            INTEGER :: indminor(ncol,nlay)
            REAL(KIND=r8) :: minorfrac(ncol,nlay)
            REAL(KIND=r8) :: scaleminor(ncol,nlay)
            REAL(KIND=r8) :: scaleminorn2(ncol,nlay)
            REAL(KIND=r8) :: fac01(ncol,nlay)
            REAL(KIND=r8) :: fac10(ncol,nlay)
            REAL(KIND=r8) :: fac11(ncol,nlay)
            REAL(KIND=r8) :: fac00(ncol,nlay) !
            REAL(KIND=r8) :: rat_o3co2_1(ncol,nlay)
            REAL(KIND=r8) :: rat_o3co2(ncol,nlay)
            REAL(KIND=r8) :: rat_h2och4(ncol,nlay)
            REAL(KIND=r8) :: rat_h2oo3(ncol,nlay)
            REAL(KIND=r8) :: rat_h2och4_1(ncol,nlay)
            REAL(KIND=r8) :: rat_h2oo3_1(ncol,nlay)
            REAL(KIND=r8) :: rat_h2oco2(ncol,nlay)
            REAL(KIND=r8) :: rat_n2oco2(ncol,nlay)
            REAL(KIND=r8) :: rat_h2on2o(ncol,nlay)
            REAL(KIND=r8) :: rat_n2oco2_1(ncol,nlay)
            REAL(KIND=r8) :: rat_h2oco2_1(ncol,nlay)
            REAL(KIND=r8) :: rat_h2on2o_1(ncol,nlay) !
            ! Atmosphere/clouds - cldprop
            INTEGER :: ncbands(ncol) ! number of cloud spectral bands
            INTEGER :: inflag(ncol) ! flag for cloud property method
            INTEGER :: iceflag(ncol) ! flag for ice cloud properties
            INTEGER :: liqflag(ncol) ! flag for liquid cloud properties
            ! Atmosphere/clouds - cldprmc [mcica]
            REAL(KIND=r8) :: cldfmc(ncol,ngptlw,nlay) ! cloud fraction [mcica]
            REAL(KIND=r8) :: ciwpmc(ncol,ngptlw,nlay) ! cloud ice water path [mcica]
            REAL(KIND=r8) :: clwpmc(ncol,ngptlw,nlay) ! cloud liquid water path [mcica]
            REAL(KIND=r8) :: relqmc(ncol,nlay) ! liquid particle size (microns)
            REAL(KIND=r8) :: reicmc(ncol,nlay) ! ice particle effective radius (microns)
            REAL(KIND=r8) :: dgesmc(ncol,nlay) ! ice particle generalized effective size (microns)
            REAL(KIND=r8) :: taucmc(ncol,ngptlw,nlay) ! cloud optical depth [mcica]
            !      real(kind=r8) :: ssacmc(ngptlw,nlay)     ! cloud single scattering albedo [mcica]
            !   for future expansion
            !   (lw scattering not yet available)
            !      real(kind=r8) :: asmcmc(ngptlw,nlay)     ! cloud asymmetry parameter [mcica]
            !   for future expansion
            !   (lw scattering not yet available)
            ! Output
            REAL(KIND=r8) :: totuflux(ncol,0:nlay) ! upward longwave flux (w/m2)
            REAL(KIND=r8) :: totdflux(ncol,0:nlay) ! downward longwave flux (w/m2)
            REAL(KIND=r8) :: totufluxs(ncol,nbndlw,0:nlay) ! upward longwave flux spectral (w/m2)
            REAL(KIND=r8) :: totdfluxs(ncol,nbndlw,0:nlay) ! downward longwave flux spectral (w/m2)
            REAL(KIND=r8) :: fnet(ncol,0:nlay) ! net longwave flux (w/m2)
            REAL(KIND=r8) :: htr(ncol,0:nlay) ! longwave heating rate (k/day)
            REAL(KIND=r8) :: totuclfl(ncol,0:nlay) ! clear sky upward longwave flux (w/m2)
            REAL(KIND=r8) :: totdclfl(ncol,0:nlay) ! clear sky downward longwave flux (w/m2)
            REAL(KIND=r8) :: fnetc(ncol,0:nlay) ! clear sky net longwave flux (w/m2)
            REAL(KIND=r8) :: htrc(ncol,0:nlay) ! clear sky longwave heating rate (k/day)
            ! Initializations
      oneminus = 1._r8 - 1.e-6_r8
      pi = 2._r8 * asin(1._r8)
      fluxfac = pi * 2.e4_r8                    ! orig:   fluxfac = pi * 2.d4 ! orig:   fluxfac = pi * 2.d4
      istart = 1
      iend = 16
      iout = 0
      ims = 1
            ! Set imca to select calculation type:
            !  imca = 0, use standard forward model calculation
            !  imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
            ! *** This version uses McICA (imca = 1) ***
            ! Set icld to select of clear or cloud calculation and cloud overlap method
            ! icld = 0, clear only
            ! icld = 1, with clouds using random cloud overlap
            ! icld = 2, with clouds using maximum/random cloud overlap
            ! icld = 3, with clouds using maximum cloud overlap (McICA only)
      if (icld.lt.0.or.icld.gt.3) icld = 2
            ! Set iaer to select aerosol option
            ! iaer = 0, no aerosols
            ! iaer = 10, input total aerosol optical depth (tauaer) directly
      iaer = 10
            !Call model and data initialization, compute lookup tables, perform
            ! reduction of g-points from 256 to 140 for input absorption coefficient
            ! data and other arrays.
            !
            ! In a GCM this call should be placed in the model initialization
            ! area, since this has to be called only once.
            !      call rrtmg_lw_ini
            !  This is the main longitude/column loop within RRTMG.
      do iplon = 1, ncol
                !  Prepare atmospheric profile from GCM for use in RRTMG, and define
                !  other input parameters.
         call inatm (iplon, nlay, icld, iaer, &
              play, plev, tlay, tlev, tsfc, h2ovmr, &
              o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, cfc11vmr, cfc12vmr, &
              cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, &
              cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, &
              pavel(iplon,:), pz(iplon,:), tavel(iplon,:), tz(iplon,:), tbound(iplon), semiss(iplon,:), coldry(iplon,:), &
              wkl(iplon,:,:), wbrodl(iplon,:), wx(iplon,:,:), pwvcm(iplon), inflag(iplon), iceflag(iplon), liqflag(iplon), &
              cldfmc(iplon,:,:), taucmc(iplon,:,:), ciwpmc(iplon,:,:), clwpmc(iplon,:,:), reicmc(iplon,:), dgesmc(iplon,:), relqmc(iplon,:), taua(iplon,:,:))
      end do       
      do iplon = 1, ncol
                !  For cloudy atmosphere, use cldprop to set cloud optical properties based on
                !  input cloud physical properties.  Select method based on choices described
                !  in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
                !  effective radius must be passed into cldprop.  Cloud fraction and cloud
                !  optical depth are transferred to rrtmg_lw arrays in cldprop.
         call cldprmc(nlay, inflag(iplon), iceflag(iplon), liqflag(iplon), cldfmc(iplon,:,:), ciwpmc(iplon,:,:), &
                      clwpmc(iplon,:,:), reicmc(iplon,:), dgesmc(iplon,:), relqmc(iplon,:), ncbands(iplon), taucmc(iplon,:,:))
                ! Calculate information needed by the radiative transfer routine
                ! that is specific to this atmosphere, especially some of the
                ! coefficients and indices needed to compute the optical depths
                ! by interpolating data from stored reference atmospheres.
      end do       
      do iplon = 1, ncol
         call setcoef(nlay, istart, pavel(iplon,:), tavel(iplon,:), tz(iplon,:), tbound(iplon), semiss(iplon,:), &
                      coldry(iplon,:), wkl(iplon,:,:), wbrodl(iplon,:), &
                      laytrop(iplon), jp(iplon,:), jt(iplon,:), jt1(iplon,:), planklay(iplon,:,:), planklev(iplon,:,:), plankbnd(iplon,:), &
                      colh2o(iplon,:), colco2(iplon,:), colo3(iplon,:), coln2o(iplon,:), colco(iplon,:), colch4(iplon,:), colo2(iplon,:), &
                      colbrd(iplon,:), fac00(iplon,:), fac01(iplon,:), fac10(iplon,:), fac11(iplon,:), &
                      rat_h2oco2(iplon,:), rat_h2oco2_1(iplon,:), rat_h2oo3(iplon,:), rat_h2oo3_1(iplon,:), &
                      rat_h2on2o(iplon,:), rat_h2on2o_1(iplon,:), rat_h2och4(iplon,:), rat_h2och4_1(iplon,:), &
                      rat_n2oco2(iplon,:), rat_n2oco2_1(iplon,:), rat_o3co2(iplon,:), rat_o3co2_1(iplon,:), &
                      selffac(iplon,:), selffrac(iplon,:), indself(iplon,:), forfac(iplon,:), forfrac(iplon,:), indfor(iplon,:), &
                      minorfrac(iplon,:), scaleminor(iplon,:), scaleminorn2(iplon,:), indminor(iplon,:))
                !  Calculate the gaseous optical depths and Planck fractions for
                !  each longwave spectral band.
      end do       
      do iplon = 1, ncol
         call taumol(nlay, pavel(iplon,:), wx(iplon,:,:), coldry(iplon,:), &
                     laytrop(iplon), jp(iplon,:), jt(iplon,:), jt1(iplon,:), planklay(iplon,:,:), planklev(iplon,:,:), plankbnd(iplon,:), &
                     colh2o(iplon,:), colco2(iplon,:), colo3(iplon,:), coln2o(iplon,:), colco(iplon,:), colch4(iplon,:), colo2(iplon,:), &
                     colbrd(iplon,:), fac00(iplon,:), fac01(iplon,:), fac10(iplon,:), fac11(iplon,:), &
                     rat_h2oco2(iplon,:), rat_h2oco2_1(iplon,:), rat_h2oo3(iplon,:), rat_h2oo3_1(iplon,:), &
                     rat_h2on2o(iplon,:), rat_h2on2o_1(iplon,:), rat_h2och4(iplon,:), rat_h2och4_1(iplon,:), &
                     rat_n2oco2(iplon,:), rat_n2oco2_1(iplon,:), rat_o3co2(iplon,:), rat_o3co2_1(iplon,:), &
                     selffac(iplon,:), selffrac(iplon,:), indself(iplon,:), forfac(iplon,:), forfrac(iplon,:), indfor(iplon,:), &
                     minorfrac(iplon,:), scaleminor(iplon,:), scaleminorn2(iplon,:), indminor(iplon,:), &
                     fracs(iplon,:,:), taug(iplon,:,:))
                ! Combine gaseous and aerosol optical depths, if aerosol active
      end do       
      do iplon = 1, ncol
         if (iaer .eq. 0) then
            do k = 1, nlay
               do ig = 1, ngptlw 
                  taut(iplon,k,ig) = taug(iplon,k,ig)
               enddo
            enddo
         elseif (iaer .eq. 10) then
            do k = 1, nlay
               do ig = 1, ngptlw 
                  taut(iplon,k,ig) = taug(iplon,k,ig) + taua(iplon,k,ngb(ig))
               enddo
            enddo
         endif
                ! Call the radiative transfer routine.
                ! Either routine can be called to do clear sky calculation.  If clouds
                ! are present, then select routine based on cloud overlap assumption
                ! to be used.  Clear sky calculation is done simultaneously.
                ! For McICA, RTRNMC is called for clear and cloudy calculations.
         call rtrnmc(nlay, istart, iend, iout, pz(iplon,:), semiss(iplon,:), ncbands(iplon), &
                     cldfmc(iplon,:,:), taucmc(iplon,:,:), planklay(iplon,:,:), planklev(iplon,:,:), plankbnd(iplon,:), &
                     pwvcm(iplon), fracs(iplon,:,:), taut(iplon,:,:), &
                     totuflux(iplon,:), totdflux(iplon,:), fnet(iplon,:), htr(iplon,:), &
                     totuclfl(iplon,:), totdclfl(iplon,:), fnetc(iplon,:), htrc(iplon,:), totufluxs(iplon,:,:), totdfluxs(iplon,:,:) )
                !  Transfer up and down fluxes and heating rate to output arrays.
                !  Vertical indexing goes from bottom to top
         do k = 0, nlay
            uflx(iplon,k+1) = totuflux(iplon,k)
            dflx(iplon,k+1) = totdflux(iplon,k)
            uflxc(iplon,k+1) = totuclfl(iplon,k)
            dflxc(iplon,k+1) = totdclfl(iplon,k)
            uflxs(:,iplon,k+1) = totufluxs(iplon,:,k)
            dflxs(:,iplon,k+1) = totdfluxs(iplon,:,k)
         enddo
         do k = 0, nlay-1
            hr(iplon,k+1) = htr(iplon,k)
            hrc(iplon,k+1) = htrc(iplon,k)
         enddo
      enddo
        END SUBROUTINE rrtmg_lw
        !***************************************************************************

        SUBROUTINE inatm(iplon, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, o2vmr, n2ovmr, &
        cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, &
        relqmcl, tauaer, pavel, pz, tavel, tz, tbound, semiss, coldry, wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag, cldfmc, &
        taucmc, ciwpmc, clwpmc, reicmc, dgesmc, relqmc, taua)
            !***************************************************************************
            !
            !  Input atmospheric profile from GCM, and prepare it for use in RRTMG_LW.
            !  Set other RRTMG_LW input parameters.
            !
            !***************************************************************************
            ! --------- Modules ----------
            USE parrrtm, ONLY: nmol
            USE parrrtm, ONLY: maxxsec
            USE parrrtm, ONLY: nbndlw
            USE parrrtm, ONLY: ngptlw
            USE rrlw_con, ONLY: grav
            USE rrlw_con, ONLY: avogad
            USE rrlw_wvn, ONLY: ixindx
            ! ------- Declarations -------
            ! ----- Input -----
            INTEGER, intent(in) :: iplon ! column loop index
            INTEGER, intent(in) :: nlay ! Number of model layers
            INTEGER, intent(in) :: icld ! clear/cloud and cloud overlap flag
            INTEGER, intent(in) :: iaer ! aerosol option flag
            REAL(KIND=r8), intent(in) :: play(:,:) ! Layer pressures (hPa, mb)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: plev(:,:) ! Interface pressures (hPa, mb)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(in) :: tlay(:,:) ! Layer temperatures (K)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: tlev(:,:) ! Interface temperatures (K)
            !    Dimensions: (ncol,nlay+1)
            REAL(KIND=r8), intent(in) :: tsfc(:) ! Surface temperature (K)
            !    Dimensions: (ncol)
            REAL(KIND=r8), intent(in) :: h2ovmr(:,:) ! H2O volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: o3vmr(:,:) ! O3 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: co2vmr(:,:) ! CO2 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: ch4vmr(:,:) ! Methane volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: o2vmr(:,:) ! O2 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: n2ovmr(:,:) ! Nitrous oxide volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: cfc11vmr(:,:) ! CFC11 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: cfc12vmr(:,:) ! CFC12 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: cfc22vmr(:,:) ! CFC22 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: ccl4vmr(:,:) ! CCL4 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: emis(:,:) ! Surface emissivity
            !    Dimensions: (ncol,nbndlw)
            INTEGER, intent(in) :: inflglw ! Flag for cloud optical properties
            INTEGER, intent(in) :: iceflglw ! Flag for ice particle specification
            INTEGER, intent(in) :: liqflglw ! Flag for liquid droplet specification
            REAL(KIND=r8), intent(in) :: cldfmcl(:,:,:) ! Cloud fraction
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: ciwpmcl(:,:,:) ! Cloud ice water path (g/m2)
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: clwpmcl(:,:,:) ! Cloud liquid water path (g/m2)
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: reicmcl(:,:) ! Cloud ice effective radius (microns)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: relqmcl(:,:) ! Cloud water drop effective radius (microns)
            !    Dimensions: (ncol,nlay)
            REAL(KIND=r8), intent(in) :: taucmcl(:,:,:) ! Cloud optical depth
            !    Dimensions: (ngptlw,ncol,nlay)
            REAL(KIND=r8), intent(in) :: tauaer(:,:,:) ! Aerosol optical depth
            !    Dimensions: (ncol,nlay,nbndlw)
            ! ----- Output -----
            ! Atmosphere
            REAL(KIND=r8), intent(out) :: pavel(:) ! layer pressures (mb)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: tavel(:) ! layer temperatures (K)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: pz(0:) ! level (interface) pressures (hPa, mb)
            !    Dimensions: (0:nlay)
            REAL(KIND=r8), intent(out) :: tz(0:) ! level (interface) temperatures (K)
            !    Dimensions: (0:nlay)
            REAL(KIND=r8), intent(out) :: tbound ! surface temperature (K)
            REAL(KIND=r8), intent(out) :: coldry(:) ! dry air column density (mol/cm2)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: wbrodl(:) ! broadening gas column density (mol/cm2)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: wkl(:,:) ! molecular amounts (mol/cm-2)
            !    Dimensions: (mxmol,nlay)
            REAL(KIND=r8), intent(out) :: wx(:,:) ! cross-section amounts (mol/cm-2)
            !    Dimensions: (maxxsec,nlay)
            REAL(KIND=r8), intent(out) :: pwvcm ! precipitable water vapor (cm)
            REAL(KIND=r8), intent(out) :: semiss(:) ! lw surface emissivity
            !    Dimensions: (nbndlw)
            ! Atmosphere/clouds - cldprop
            INTEGER, intent(out) :: inflag ! flag for cloud property method
            INTEGER, intent(out) :: iceflag ! flag for ice cloud properties
            INTEGER, intent(out) :: liqflag ! flag for liquid cloud properties
            REAL(KIND=r8), intent(out) :: cldfmc(:,:) ! cloud fraction [mcica]
            !    Dimensions: (ngptlw,nlay)
            REAL(KIND=r8), intent(out) :: ciwpmc(:,:) ! cloud ice water path [mcica]
            !    Dimensions: (ngptlw,nlay)
            REAL(KIND=r8), intent(out) :: clwpmc(:,:) ! cloud liquid water path [mcica]
            !    Dimensions: (ngptlw,nlay)
            REAL(KIND=r8), intent(out) :: relqmc(:) ! liquid particle effective radius (microns)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: reicmc(:) ! ice particle effective radius (microns)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: dgesmc(:) ! ice particle generalized effective size (microns)
            !    Dimensions: (nlay)
            REAL(KIND=r8), intent(out) :: taucmc(:,:) ! cloud optical depth [mcica]
            !    Dimensions: (ngptlw,nlay)
            REAL(KIND=r8), intent(out) :: taua(:,:) ! Aerosol optical depth
            ! Dimensions: (nlay,nbndlw)
            ! ----- Local -----
            REAL(KIND=r8), parameter :: amd = 28.9660_r8 ! Effective molecular weight of dry air (g/mol)
            REAL(KIND=r8), parameter :: amw = 18.0160_r8 ! Molecular weight of water vapor (g/mol)
            !      real(kind=r8), parameter :: amc = 44.0098_r8      ! Molecular weight of carbon dioxide (g/mol)
            !      real(kind=r8), parameter :: amo = 47.9998_r8      ! Molecular weight of ozone (g/mol)
            !      real(kind=r8), parameter :: amo2 = 31.9999_r8     ! Molecular weight of oxygen (g/mol)
            !      real(kind=r8), parameter :: amch4 = 16.0430_r8    ! Molecular weight of methane (g/mol)
            !      real(kind=r8), parameter :: amn2o = 44.0128_r8    ! Molecular weight of nitrous oxide (g/mol)
            !      real(kind=r8), parameter :: amc11 = 137.3684_r8   ! Molecular weight of CFC11 (g/mol) - CCL3F
            !      real(kind=r8), parameter :: amc12 = 120.9138_r8   ! Molecular weight of CFC12 (g/mol) - CCL2F2
            !      real(kind=r8), parameter :: amc22 = 86.4688_r8    ! Molecular weight of CFC22 (g/mol) - CHCLF2
            !      real(kind=r8), parameter :: amcl4 = 153.823_r8    ! Molecular weight of CCL4 (g/mol) - CCL4
            ! Set molecular weight ratios (for converting mmr to vmr)
            !  e.g. h2ovmr = h2ommr * amdw)
            ! Molecular weight of dry air / water vapor
            ! Molecular weight of dry air / carbon dioxide
            ! Molecular weight of dry air / ozone
            ! Molecular weight of dry air / methane
            ! Molecular weight of dry air / nitrous oxide
            ! Molecular weight of dry air / CFC11
            ! Molecular weight of dry air / CFC12
            ! Stefan-Boltzmann constant (W/m2K4)
            INTEGER :: l
            INTEGER :: imol
            INTEGER :: ix
            INTEGER :: n
            INTEGER :: ib
            INTEGER :: ig ! Loop indices
            REAL(KIND=r8) :: amttl
            REAL(KIND=r8) :: wvttl
            REAL(KIND=r8) :: amm
            REAL(KIND=r8) :: summol
            REAL(KIND=r8) :: wvsh
            !  Initialize all molecular amounts and cloud properties to zero here, then pass input amounts
            !  into RRTM arrays below.
      wkl(:,:) = 0.0_r8
      wx(:,:) = 0.0_r8
      cldfmc(:,:) = 0.0_r8
      taucmc(:,:) = 0.0_r8
      ciwpmc(:,:) = 0.0_r8
      clwpmc(:,:) = 0.0_r8
      reicmc(:) = 0.0_r8
      dgesmc(:) = 0.0_r8
      relqmc(:) = 0.0_r8
      taua(:,:) = 0.0_r8
      amttl = 0.0_r8
      wvttl = 0.0_r8
            !  Set surface temperature.
      tbound = tsfc(iplon)
            !  Install input GCM arrays into RRTMG_LW arrays for pressure, temperature,
            !  and molecular amounts.
            !  Pressures are input in mb, or are converted to mb here.
            !  Molecular amounts are input in volume mixing ratio, or are converted from
            !  mass mixing ratio (or specific humidity for h2o) to volume mixing ratio
            !  here. These are then converted to molecular amount (molec/cm2) below.
            !  The dry air column COLDRY (in molec/cm2) is calculated from the level
            !  pressures, pz (in mb), based on the hydrostatic equation and includes a
            !  correction to account for h2o in the layer.  The molecular weight of moist
            !  air (amm) is calculated for each layer.
            !  Note: In RRTMG, layer indexing goes from bottom to top, and coding below
            !  assumes GCM input fields are also bottom to top. Input layer indexing
            !  from GCM fields should be reversed here if necessary.
      pz(0) = plev(iplon,nlay+1)
      tz(0) = tlev(iplon,nlay+1)
      do l = 1, nlay
         pavel(l) = play(iplon,nlay-l+1)
         tavel(l) = tlay(iplon,nlay-l+1)
         pz(l) = plev(iplon,nlay-l+1)
         tz(l) = tlev(iplon,nlay-l+1)
                ! For h2o input in vmr:
         wkl(1,l) = h2ovmr(iplon,nlay-l+1)
                ! For h2o input in mmr:
                !         wkl(1,l) = h2o(iplon,nlay-l)*amdw
                ! For h2o input in specific humidity;
                !         wkl(1,l) = (h2o(iplon,nlay-l)/(1._r8 - h2o(iplon,nlay-l)))*amdw
         wkl(2,l) = co2vmr(iplon,nlay-l+1)
         wkl(3,l) = o3vmr(iplon,nlay-l+1)
         wkl(4,l) = n2ovmr(iplon,nlay-l+1)
         wkl(6,l) = ch4vmr(iplon,nlay-l+1)
         wkl(7,l) = o2vmr(iplon,nlay-l+1)
         amm = (1._r8 - wkl(1,l)) * amd + wkl(1,l) * amw            
         coldry(l) = (pz(l-1)-pz(l)) * 1.e3_r8 * avogad / &
                     (1.e2_r8 * grav * amm * (1._r8 + wkl(1,l)))
                ! Set cross section molecule amounts from input; convert to vmr if necessary
         wx(1,l) = ccl4vmr(iplon,nlay-l+1)
         wx(2,l) = cfc11vmr(iplon,nlay-l+1)
         wx(3,l) = cfc12vmr(iplon,nlay-l+1)
         wx(4,l) = cfc22vmr(iplon,nlay-l+1)
      enddo
      coldry(nlay) = (pz(nlay-1)) * 1.e3_r8 * avogad / &
                        (1.e2_r8 * grav * amm * (1._r8 + wkl(1,nlay-1)))
            ! At this point all molecular amounts in wkl and wx are in volume mixing ratio;
            ! convert to molec/cm2 based on coldry for use in rrtm.  also, compute precipitable
            ! water vapor for diffusivity angle adjustments in rtrn and rtrnmr.
      do l = 1, nlay
         summol = 0.0_r8
         do imol = 2, nmol
            summol = summol + wkl(imol,l)
         enddo
         wbrodl(l) = coldry(l) * (1._r8 - summol)
         do imol = 1, nmol
            wkl(imol,l) = coldry(l) * wkl(imol,l)
         enddo
         amttl = amttl + coldry(l)+wkl(1,l)
         wvttl = wvttl + wkl(1,l)
         do ix = 1,maxxsec
            if (ixindx(ix) .ne. 0) then
               wx(ixindx(ix),l) = coldry(l) * wx(ix,l) * 1.e-20_r8
            endif
         enddo
      enddo
      wvsh = (amw * wvttl) / (amd * amttl)
      pwvcm = wvsh * (1.e3_r8 * pz(0)) / (1.e2_r8 * grav)
            ! Set spectral surface emissivity for each longwave band.
      do n=1,nbndlw
         semiss(n) = emis(iplon,n)
                !          semiss(n) = 1.0_r8
      enddo
            ! Transfer aerosol optical properties to RRTM variable;
            ! modify to reverse layer indexing here if necessary.
      if (iaer .ge. 1) then 
         do l = 1, nlay-1
            do ib = 1, nbndlw
               taua(l,ib) = tauaer(iplon,nlay-l,ib)
            enddo
         enddo
      endif
            ! Transfer cloud fraction and cloud optical properties to RRTM variables,
            ! modify to reverse layer indexing here if necessary.
      if (icld .ge. 1) then 
         inflag = inflglw
         iceflag = iceflglw
         liqflag = liqflglw
                ! Move incoming GCM cloud arrays to RRTMG cloud arrays.
                ! For GCM input, incoming reice is in effective radius; for Fu parameterization (iceflag = 3)
                ! convert effective radius to generalized effective size using method of Mitchell, JAS, 2002:
         do l = 1, nlay-1
            do ig = 1, ngptlw
               cldfmc(ig,l) = cldfmcl(ig,iplon,nlay-l)
               taucmc(ig,l) = taucmcl(ig,iplon,nlay-l)
               ciwpmc(ig,l) = ciwpmcl(ig,iplon,nlay-l)
               clwpmc(ig,l) = clwpmcl(ig,iplon,nlay-l)
            enddo
            reicmc(l) = reicmcl(iplon,nlay-l)
            if (iceflag .eq. 3) then
               dgesmc(l) = 1.5396_r8 * reicmcl(iplon,nlay-l)
            endif
            relqmc(l) = relqmcl(iplon,nlay-l)
         enddo
                ! If an extra layer is being used in RRTMG, set all cloud properties to zero in the extra layer.
         cldfmc(:,nlay) = 0.0_r8
         taucmc(:,nlay) = 0.0_r8
         ciwpmc(:,nlay) = 0.0_r8
         clwpmc(:,nlay) = 0.0_r8
         reicmc(nlay) = 0.0_r8
         dgesmc(nlay) = 0.0_r8
         relqmc(nlay) = 0.0_r8
         taua(nlay,:) = 0.0_r8
      endif
        END SUBROUTINE inatm
    END MODULE rrtmg_lw_rad
module functions

    Implicit none
     
contains

subroutine write_ini_file_for_testing_precision(Cl_flag,bessel,q,kmaxtau0,index)
    use fiducial
    Implicit none

    Real*8:: bessel,q,kmaxtau0
    Real*8,dimension(nbins):: z_bin_centers, z_bin_widths, z_bin_bias, s_z_mag_bias
    logical :: Cl_flag,fid1,fid2,fid3,fiducial_flag
    character(len=*),parameter :: fmt = '(i2.2)' 
    character*16 :: string
    Logical,parameter :: lensing_flag = .true. 
    Integer*4 :: index

    fid1 = bessel .eq. selection_sampling_bessel_fid

    fid2 =  q .eq. q_linstep_fid 

    fid3 = kmaxtau0 .eq. k_max_tau0_over_l_max_fid 

    fiducial_flag = (fid1 .and. fid2) .and. fid3 
     
    call bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)

    write(string,fmt) index

    If (lensing_flag) then

        If (fiducial_flag) then

            If (Cl_flag) then

                print *,'PRECISION PARAMETERS MUST BE DIFFERENT OF FIDUCIAL VALUES FOR CL'

                stop

            Else

               print *,'ERROR FILE IS ASSUMED TO BE PRECOMPUTED'

               stop

!                open(10, file='./ini_files/El.ini')

 !               write(10,*) 'root = ../data/El_nl_'

  !              write(10,'(a25)') 'number count error = 0.10'   

            End If

        Else

            If (Cl_flag) then

                open(10, file='./ini_files/Cl_'//trim(string)//'.ini')

                write(10,*) 'root = ../data/Cl_'//trim(string)//'_'

            Else
               
                print *,'NOT ERROR FILE WITHOUT FIDUCIAL PARAMETERS'

                stop

            End If


        End If

        write(10,'(a59)') 'number count contributions = density, rsd, doppler, lensing'

    Else 

        If (fiducial_flag) then

            If (Cl_flag) then

                print *,'PRECISION PARAMETERS MUST BE DIFFERENT OF FIDUCIAL VALUES FOR CL'

                stop

            Else

                open(10, file='./ini_files/El_nl.ini')

                write(10,*) 'root = ../data/El_nl_'

                write(10,'(a25)') 'number count error = 0.10'   

            End If

        Else

            If (Cl_flag) then

                open(10, file='./ini_files/Cl_'//trim(string)//'.ini')

                write(10,*) 'root = ../data/Cl_'//trim(string)//'_'

            Else
               
                print *,'NOT ERROR FILE WITHOUT FIDUCIAL PARAMETERS'

                stop

            End If


        End If

        write(10,'(a50)') 'number count contributions = density, rsd, doppler'

    End if
    
    ! Background parameters and anisotropic stress

    write(10,'(a6, es16.10)') 'A_s = ', A_s  

    write(10,'(a6, es16.10)') 'n_s = ', n_s

    write(10,'(a5, es16.10)') 'H0 = ', H0

    write(10,'(a10, es16.10)') 'omega_b = ', omega_b

    write(10,'(a12, es16.10)') 'omega_cdm = ', omega_cdm

    write(10,'(a11, es16.10)') 'tau_reio = ', tau

    write(10,'(a11, es16.10)') 'MG_beta2 = ', MG_beta2

    ! Parameters for massive neutrinos                                                                                            

    write(10,'(a7, f5.3)') 'N_ur = ', real(N_ur)

    write(10,'(a9, f5.3)') 'N_ncdm = ', real(N_ncdm)

    write(10,'(a11, f5.3)') 'deg_ncdm = ', real(deg_ncdm)

    write(10,'(a9, es16.10)') 'm_ncdm = ', m_ncdm

    ! Number counts in the output                                                                                            

    write(10,'(a12)') 'output = nCl'
    
!    write(10,'(a20)') 'non linear = halofit'
 
    write(10,'(a32)') 'dNdz_selection = analytic_euclid'

    write(10,'(a17)') 'dNdz_evolution = '

    write(10,'(a20)') 'selection = gaussian'

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',', z_bin_centers(3),',',&
    z_bin_centers(4),',',z_bin_centers(5)!,',',z_bin_centers(6),',',z_bin_centers(7),',',z_bin_centers(8),',',&
    !z_bin_centers(9),',',z_bin_centers(10)

    write(10,'(a18, 4(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',z_bin_widths(3),',',&
    z_bin_widths(4),',',z_bin_widths(5)!,',',z_bin_widths(6),',',z_bin_widths(7),',',z_bin_widths(8),',',&
!    z_bin_widths(9),',',z_bin_widths(10)

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',z_bin_bias(3),',',&
    z_bin_bias(4),',',z_bin_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a31, 4(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',s_z_mag_bias(2),',',&
         s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a15,i2)') 'non_diagonal = ',nbins-1

    write(10,'(a13)') 'headers = yes'

    write(10,'(a17)') 'bessel file = yes'

    write(10,'(a12,i4)') 'l_max_lss = ', lmax_class

    write(10,'(a8,i1)') 'l_min = ', lmin

    write(10,'(a27,f2.0)') 'selection_magnitude_bias = ', 0.

    write(10,'(a14)') 'format = class'

    write(10,'(a17)') 'gauge = newtonian'

    ! PRECISION PARAMETERS

    write(10,'(a40, f6.0)') 'l_switch_limber_for_cl_density_over_z = ', real(l_switch_limber_for_cl_density_over_z)

    write(10,'(a28, f5.2)') 'selection_sampling_bessel = ', real(bessel)

    write(10,'(a12, f5.1)') 'q_linstep = ', real(q)

    write(10,'(a24, f5.2)') 'k_max_tau0_over_l_max = ', real(kmaxtau0)

    close(10)

end subroutine write_ini_file_for_testing_precision

subroutine run_class_testing_precision(Cl_flag,index)
    use fiducial
    Implicit none

    character*16 :: string
    logical :: exist,Cl_flag
    character(len=*),parameter :: fmt = '(i2.2)'
!    Logical,parameter :: lensing_flag = .true.
    Integer*4 :: index
    
    write(string,fmt) index

!    If (.not.lensing_flag) then

    If (Cl_flag) then

       inquire(file='./data/Cl_'//trim(string)//'_cl.dat',exist=exist)
       
       If (.not.exist) then

          call write_sh_file('Cl_'//trim(string)//'')

          call system('cd class_montanari-lensing ; sbatch Cl_'//trim(string)//'.sh')

       End If

    Else

       inquire(file='./data/El_nl_cl.dat',exist=exist)

       If (.not.exist) then

          call write_sh_file('El_nl')

          call system('cd class_montanari-lensing ; sbatch El_nl.sh')

       End If

    End if

!    Else 

!        print *, 'TESTING PRECISION HAS ONLY BEEN IMPLEMENTED WITHOUT LENSING'

!        stop

!    End if

end subroutine run_class_testing_precision

subroutine compute_data_for_testing_precision()

    use fiducial
    Implicit none

    Integer*4,parameter :: number_of_q = 10
    Integer*4,parameter :: number_of_kmax = 0
    Integer*4,parameter :: number_of_bessel = 0
    Integer*4           :: index
    Real*8,parameter    :: step_q = 10.0d0
    Real*8,parameter    :: step_kmax = 2.d0
    Real*8,parameter    :: step_bessel = 0.3d0
    Real*8              :: ssb,qls,kmt
    Real*8,parameter    :: kmt_fid = 2.d0 ! = k_max_tau0_over_l_max_fid
    Real*8,parameter    :: ssb_fid = 1.2d0 ! = selection_sampling_bessel_fid
    Real*8,parameter    :: qls_fid = 2.3d0 != q_linstep_fid

    ! UNCOMMENT TO COMPUTE ERROR FILE
!    call write_ini_file_for_testing_precision(.false.,selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid,0)

!    call run_class_testing_precision(.false.,0)

    Do index=1,number_of_q+number_of_kmax+number_of_bessel

        If (index .le. number_of_q) then

            qls = qls_fid +  dble(index)*step_q

            call write_ini_file_for_testing_precision(.true.,ssb_fid,qls,kmt_fid,index)

            call run_class_testing_precision(.true.,index)

        Else If ((index .gt. number_of_q) .and. (index .le. (number_of_q+number_of_kmax) )) then

            kmt = kmt_fid - dble(index-number_of_q)*step_kmax

            call write_ini_file_for_testing_precision(.true.,ssb_fid,qls_fid,kmt,index)

            call run_class_testing_precision(.true.,index)

        Else If ( (index .gt. (number_of_q+number_of_kmax)) .and. (index .le. (number_of_q+number_of_kmax+number_of_bessel) ) )then

            ssb = ssb_fid - dble(index-number_of_q-number_of_kmax)*step_bessel

            call write_ini_file_for_testing_precision(.true.,ssb,qls_fid,kmt_fid,index)

            call run_class_testing_precision(.true.,index)

        End If

    End Do

end subroutine compute_data_for_testing_precision

subroutine testing_precision_cl()

    use fiducial
    use arrays
    Implicit none 

    Character(len=15)                      :: filetype     ! AUXILIARY VARIABLE 
    Character*16                           :: ElCl         ! ALLOWS TO CHOOSE EITHER EL OR CL

    Integer*4,parameter :: number_of_q = 10
    Integer*4,parameter :: number_of_kmax = 0
    Integer*4,parameter :: number_of_bessel = 0
    Integer*4           :: index
    Real*8,parameter    :: step_q = 10.0d0
    Real*8,parameter    :: step_kmax = 2.d0
    Real*8,parameter    :: step_bessel = 0.3d0
    Real*8              :: ssb,qls,kmt
    Real*8,parameter    :: kmt_fid = 2.d0 ! = k_max_tau0_over_l_max_fid
    Real*8,parameter    :: ssb_fid = 1.2d0 ! = selection_sampling_bessel_fid
    Real*8,parameter    :: qls_fid = 2.3d0 != q_linstep_fid


    ! ALLOCATING MEMORY FOR EL, FIDUCIAL CL (LENSING), FIDUCIAL CL (NOT LENSING), OBSERVED CL, SHOT NOISE, SYSTEMATIC CL
    allocate (El(lmin:lmax,0:nbins,0:nbins),Cl_fid(lmin:lmax,0:nbins,0:nbins),Cl_obs(lmin:lmax,0:nbins,0:nbins),&
         Nl(1:nbins,1:nbins),Cl_current(lmin:lmax,0:nbins,0:nbins),stat = status3)

    If (status3 .eq. 0) then 

       write(15,*) 'MEMORY FOR EL, CL_FID, CL_OBS, NL, CL_SYST ALLOCATED SUCCESSFULLY'

    Else 

       write(15,*) 'MEMORY FOR EL, CL_FID, CL_OBS, NL, CL_SYST WAS NOT ALLOCATED PROPERLY'

       stop

    End If

    call read_data(El,10,filetype,ElCl,.true.,.true.,.false.)

    call read_data(Cl_fid,10,filetype,ElCl,.true.,.true.,.true.)

    call compute_shot_noise()

    call compute_observed_Cl()

    open(16,file='./output/testing_precision_cl.dat')
    write(16,*) '# Delta \chi^2    q_linstep    k_max_tau0_over_l_max    selection_sampling_bessel '

    Do index=1,number_of_q+number_of_kmax+number_of_bessel

        If (index .le. number_of_q) then

            qls = qls_fid + dble(index)*step_q

            call read_cl(Cl_current,index,.true.)

            write(16,*) abs(euclid_galaxy_cl_likelihood(Cl_fid)-euclid_galaxy_cl_likelihood(Cl_current)),&
                 qls,kmt_fid,ssb_fid
    
        Else If ((index .gt. number_of_q) .and. (index .lt. (number_of_q+number_of_kmax) )) then

            kmt = kmt_fid - dble(index-number_of_q)*step_kmax

            call read_cl(Cl_current,index,.true.)

            write(16,*) abs(euclid_galaxy_cl_likelihood(Cl_fid)-euclid_galaxy_cl_likelihood(Cl_current)),&
                 qls_fid,kmt,ssb_fid
    
        Else If ( (index .gt. (number_of_q+number_of_kmax)) .and. (index .le. (number_of_q+number_of_kmax+number_of_bessel) ) )then

            ssb = ssb_fid - dble(index-number_of_q-number_of_kmax)*step_bessel

            call read_cl(Cl_current,index,.true.)

            write(16,*) abs(euclid_galaxy_cl_likelihood(Cl_fid)-euclid_galaxy_cl_likelihood(Cl_current)),&
                 qls_fid,kmt_fid,ssb
    
        End If

    End Do

    deallocate(El,Cl_fid,Cl_obs,Nl,Cl_current)

    close(16)

end subroutine testing_precision_cl

function log_Gaussian_likelihood(array)
    use fiducial
    Implicit none
    Integer*4 :: index
    Real*8 :: log_Gaussian_likelihood
    Real*8,dimension(number_of_parameters) :: array
    log_Gaussian_likelihood = 0.d0
    Do index=1,number_of_parameters
        log_Gaussian_likelihood = array(index)**2 + log_Gaussian_likelihood
    End Do
    log_Gaussian_likelihood = -log_Gaussian_likelihood/2.d0
end function log_Gaussian_likelihood

function euclid_galaxy_cl_likelihood(Cl)
    use fiducial
    use arrays
    Implicit none
    Integer*4,parameter :: L = lmax - lmin + 1
    Integer*4 :: indexl,indexbin_i,indexbin_j,indexbin_k,indexbin_p
    Real*8,parameter :: epsilon_min = 0.d0
    Real*8,parameter :: epsilon_max = 1.d2
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Clth,Cl,Elth
    Real*8,dimension(1:nbins,1:nbins) :: Cov_mix,Cov_obs,Cov_the,Cov_the_El,Cov_mix_new
    Real*8 :: euclid_galaxy_cl_likelihood,chi2,det_obs,det_the,det_mix,det_the_El,det_the_El_mix,epsilon_l
   
    Do indexl=lmin,lmax
        Do indexbin_i=1,nbins
            Do indexbin_j=1,nbins
                Clth(indexl,indexbin_i,indexbin_j) = 2.d0*Pi*Cl(indexl,indexbin_i,indexbin_j)/&
                dble(indexl)/(dble(indexl) + 1.d0) + Nl(indexbin_i,indexbin_j)
                Elth(indexl,indexbin_i,indexbin_j) = 2.d0*Pi*El(indexl,indexbin_i,indexbin_j)/&
                dble(indexl)/(dble(indexl) + 1.d0)*sqrt(dble(L))
            End Do
        End Do
    End Do

    chi2 = 0.d0

    Do indexl=lmin,lmax

        Do indexbin_i=1,nbins
            Do indexbin_j=1,nbins
                Cov_obs(indexbin_i,indexbin_j) = Cl_obs(indexl,indexbin_i,indexbin_j)
                Cov_the(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)
            End Do
        End Do
        
        det_obs = compute_determinant(Cov_obs)
        det_the = compute_determinant(Cov_the)
        det_mix = 0.d0
        
        If (theoreticalerror .gt. 0.d0) then 
           
            Do  indexbin_k=1,nbins

                Do indexbin_i=1,nbins
                    Do indexbin_j=1,nbins
                        Cov_mix(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)
                    End Do
                End Do
            
                Do indexbin_p=1,nbins
                    Cov_mix(indexbin_p,indexbin_k) = Cl_obs(indexl,indexbin_p,indexbin_k) 
                End Do
                
                det_mix = compute_determinant(Cov_mix) + det_mix

            End Do

            ! Here function to minimize chi2 w.r.t must be called  

            epsilon_l = 0.d0 ! In the meantime we disregard epsilon_l (output of function above)

            !If ( (epsilon_l-epsilon_min < 1.d-5/dble(L)) .or. (epsilon_max-epsilon_l<1.d-5/dble(L)) ) then 
            !    print *,'Minimization did not converge for ', indexl, 'having epsilon_l equal to ',epsilon_l
            !End If
 
            Do indexbin_i=1,nbins
                    Do indexbin_j=1,nbins
                        Cov_the_El(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)&
                        + epsilon_l*Elth(indexl,indexbin_i,indexbin_j)
                    End Do
            End Do

            det_the_El = compute_determinant(Cov_the_El)

            det_the_El_mix = 0.d0

            Do  indexbin_k=1,nbins

                Do indexbin_i=1,nbins
                    Do indexbin_j=1,nbins
                        Cov_mix_new(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)&
                        + epsilon_l*Elth(indexl,indexbin_i,indexbin_j)
                    End Do
                End Do
            
                Do indexbin_p=1,nbins
                    Cov_mix_new(indexbin_p,indexbin_k) = Cl_obs(indexl,indexbin_p,indexbin_k) 
                End Do
                
                det_the_El_mix = compute_determinant(Cov_mix_new) + det_the_El_mix

            End Do
            
            chi2 = fsky*(2.d0*dble(indexl)+1.d0)*(log(det_the_El/det_obs) + det_the_El_mix/det_the_El &
            - dble(nbins)) + chi2 + epsilon_l**2

        else

            Do  indexbin_k=1,nbins

                Do indexbin_i=1,nbins
                    Do indexbin_j=1,nbins
                        Cov_mix(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)
                    End Do
                End Do
            
                Do indexbin_p=1,nbins
                    Cov_mix(indexbin_p,indexbin_k) = Cl_obs(indexl,indexbin_p,indexbin_k) 
                End Do
                
                det_mix = compute_determinant(Cov_mix) + det_mix

            End Do

            chi2 = fsky*(2.d0*dble(indexl)+1.d0)*(log(det_the/det_obs) + det_mix/det_the - dble(nbins)) + chi2

        end if
    End Do
    
    If (abs(chi2).ge.0.d0) then

        euclid_galaxy_cl_likelihood = -chi2/2.d0   

    Else

        euclid_galaxy_cl_likelihood = -1.d10     
   
    End If


end function euclid_galaxy_cl_likelihood

function compute_determinant(A)
    use fiducial
    Implicit none
    Integer*4 :: INFO,index
    Real*8,dimension(max(1,nbins),nbins) :: A
    Integer*4,dimension(min(nbins,nbins)) :: IPIV
    !Real*8,dimension(max(1,max(1,nbins))) :: WORK

    Real*8 :: det,sgn,compute_determinant

    call dgetrf(nbins,nbins,A,nbins,IPIV,INFO)

    det = 1.d0
    Do index=1,nbins

        det = det*A(index,index)
    End Do 

    sgn = 1.d0
    Do index=1,nbins
        If (IPIV(index) .ne. index) then
            sgn = -sgn
        End If
    End Do
 
    compute_determinant = sgn*det
end function compute_determinant

subroutine compute_ratio_likelihood()
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: m,p
    Real*8,dimension(number_of_parameters) :: point_parameter_space,fiducial_point
    character(len=*),parameter :: fmt = '(8es16.10)'
    logical,parameter :: lensing_flag = .false. 
    logical :: ini_file_exist,cl_file_exist
    Real*8 :: loglikelihood
    Real*8,parameter :: step = 1.d0

    fiducial_point(1) = omega_b 
    fiducial_point(2) = omega_cdm
    fiducial_point(3) = n_s
    fiducial_point(4) = log(1.d1**1.d1*A_s)
    fiducial_point(5) = H0
!    fiducial_point(6) = m_ncdm
!    fiducial_point(7) = MG_beta2

    open(16,file='./output/ratio_likelihood_values.dat')
    write(16,*) '# -ln(L/L_max)    omega_b    omega_cdm    n_s    A_s    H0 '!   m_ncdm    MG_beta2 '
    write(16,*) -euclid_galaxy_cl_likelihood(Cl_fid_nl),omega_b,omega_cdm,n_s,A_s,&
    H0!,m_ncdm,MG_beta2

    Do p=1,2

        Do m=1,number_of_parameters

            point_parameter_space(m) = fiducial_point(m) + step*dble(p)*sigma_MG_beta2*b_lambda(m)/sqrt(sum(b_lambda(:)**2))
 
        End Do
     
        point_parameter_space(4) = exp(point_parameter_space(4))/1.d1**1.d1
       
        inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist) 

        inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

        If (cl_file_exist) then

            call system('rm ./output/current_euclid_galaxy_cl.dat')

        End If 

        If (ini_file_exist) then

            call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')

        End If

        call write_ini_file(point_parameter_space(1),point_parameter_space(2),point_parameter_space(3),&
        point_parameter_space(4),point_parameter_space(5),m_ncdm,MG_beta2,&
        tau,N_ur,N_ncdm,deg_ncdm,lensing_flag,selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

        inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist) 

        If (ini_file_exist) then

            call run_current_model(lensing_flag)
     
        End If

        If (ini_file_exist) then

            inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

            If (cl_file_exist) then

                call read_Cl(Cl_current,11,lensing_flag)

                loglikelihood = euclid_galaxy_cl_likelihood(Cl_current)

                call system('rm ./output/current_euclid_galaxy_cl.dat')    ! Remove Cl file

            Else

                write(16,*) 'Something went wrong with CLASS for current model '

                stop
  
            End If

            call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')    ! Remove ini file

        Else

            write(16,*) 'Not ini file for current model. Something went wrong with subroutine "write_ini_file" '

            stop

        End if

        write(16,*) -loglikelihood,point_parameter_space(1:number_of_parameters)!,point_parameter_space(2),&
!        point_parameter_space(3),point_parameter_space(4),point_parameter_space(5),point_parameter_space(6),&
 !       point_parameter_space(7)

    End Do

    close(16)

end subroutine compute_ratio_likelihood

subroutine compute_shot_noise()
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: m,n
    Do m = 1,nbins
        Do n = 1,nbins
            If (m .ne. n) then
                Nl(m,n) = 0.d0
            else
                Nl(m,n) = real(nbins)*(1.d0/(3600.d0*gal_per_sqarcmn*(180.d0/Pi)**2))
            End If
        End Do
    End Do
end subroutine compute_shot_noise

subroutine compute_observed_Cl()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,p,i
    Do m=lmin,lmax
        Do p=1,nbins
            Do i=1,nbins
               If (testing_precision) then
                  Cl_obs(m,p,i) = 2.d0*Pi*(Cl_fid(m,p,i) + El(m,p,i))/real(m)/(real(m)+1.d0) + Nl(p,i) 
               Else
                  Cl_obs(m,p,i) = 2.d0*Pi*(Cl_fid(m,p,i) + El(m,p,i))/real(m)/(real(m)+1.d0) + Nl(p,i) 
               End If
            End Do
        End Do
    End Do 
end subroutine compute_observed_Cl

subroutine compute_systematic_error()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,p,i
    Do m=lmin,lmax
        Do p=1,nbins
            Do i=1,nbins
                Cl_syst(m,p,i) = 2.d0*Pi*(Cl_fid_nl(m,p,i) - Cl_fid(m,p,i))/dble(m)/(dble(m)+1.d0)  
            End Do
        End Do
    End Do 
end subroutine compute_systematic_error

subroutine compute_covariance_matrix()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,i,j,p,q,Deltal
    Deltal = 1 !int(2.d0/fsky,4)
    Do m=lmin,lmax
        Do i=1,nbins
            Do j=1,nbins
                Do p=1,nbins
                    Do q=1,nbins
                        cov(m,i,j,p,q) = (Cl_obs(m,i,p)*Cl_obs(m,j,q) + &
                        Cl_obs(m,i,q)*Cl_obs(m,j,p))/fsky/Deltal/(2.d0*m + 1.d0)
                    End do
                End Do
            End Do
        End Do
    End Do
end subroutine compute_covariance_matrix

subroutine write_cov_two_indices()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,i,j,p,q
    Do m=lmin,lmax
        Do i=1,nbins 
            Do j=1,nbins
                Do p=1,nbins 
                    Do q=1,nbins
                        If ((i .le. j) .and. (p .le. q)) then
                            cov_l_IP(m,indices_I_P_from_i_j(i,j),indices_I_P_from_i_j(p,q)) = cov(m,i,j,p,q)
                        End If
                    End Do
                End Do
            End Do
        End Do
    End Do
end subroutine write_cov_two_indices

subroutine write_cov_two_indices_oa()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,i,j,p,q
    Do m=lmin,lmax
        Do i=1,nbins 
            Do j=1,nbins
                Do p=1,nbins 
                    Do q=1,nbins
                        If ((i .eq. j) .and. (p .eq. q)) then
                            cov_l_IP_oa(m,indices_I_P_from_i_j_oa(i,j),indices_I_P_from_i_j_oa(p,q)) = cov(m,i,j,p,q)
                        End If
                    End Do
                End Do
            End Do
        End Do
    End Do
end subroutine write_cov_two_indices_oa

subroutine write_inv_cov_four_indices()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,i,j
    Do m=lmin,lmax
        Do i=1,nbins*(nbins+1)/2 
            Do j=1,nbins*(nbins+1)/2
                inv_cov(m,i_j_from_I(i,1),i_j_from_I(i,2),i_j_from_I(j,1),i_j_from_I(j,2)) = inv_cov_l_IP(m,i,j)
            End Do
        End Do
    End Do
end subroutine write_inv_cov_four_indices

subroutine write_inv_cov_four_indices_oa()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,i,j
    inv_cov_oa(:,:,:,:,:) = 0.d0
    Do m=lmin,lmax
        Do i=1,nbins 
            Do j=1,nbins
                inv_cov_oa(m,i_j_from_I_oa(i,1),i_j_from_I_oa(i,2),i_j_from_I_oa(j,1),i_j_from_I_oa(j,2)) = inv_cov_l_IP_oa(m,i,j)
            End Do
        End Do
    End Do
end subroutine write_inv_cov_four_indices_oa

function i_j_from_I(m,i_or_j)
        use fiducial
        Implicit none
        Integer*4,dimension(nbins,nbins) :: I
        Integer*4 :: i_j_from_I
        Integer*4 :: p,q,counter,m,i_or_j
        logical :: found
        found = .false.
        If (m .gt. nbins*(nbins+1)/2) then 
            print *,'Argument of function "indices_i_j_from_I"  must not exceed N_bins*(N_bins+1)/2 '
            stop
        Else if ((i_or_j .gt. 2) .or. (i_or_j .lt. 1)) then
            print *, 'The argument "i_or_j" in "function i_j_from_I" must be either 1 or 2 '
            stop
        End If
        counter = 1

        Do p=1,nbins
            Do q=1,nbins
                If  (.not.found) then
                    If (p .le. q) then
                        I(p,q) = counter
                        If (m .eq. I(p,q)) then
                            If (i_or_j .eq. 1) then
                                i_j_from_I = p
                            Else if (i_or_j .eq. 2) then
                                i_j_from_I = q
                            End If
                            found = .true.
                        else
                            counter = counter + 1
                        End If
                    End If
                End If
            End Do
        End Do
end function i_j_from_I

function i_j_from_I_oa(m,i_or_j)
        use fiducial
        Implicit none
        Integer*4,dimension(nbins,nbins) :: I
        Integer*4 :: i_j_from_I_oa
        Integer*4 :: p,q,counter,m,i_or_j
        logical :: found
        found = .false.
        If (m .gt. nbins) then 
            print *,'Argument of function "indices_i_j_from_I"  must not exceed N_bins '
            stop
        Else if ((i_or_j .gt. 2) .or. (i_or_j .lt. 1)) then
            print *, 'The argument "i_or_j" in "function i_j_from_I" must be either 1 or 2 '
            stop
        End If
        counter = 1

        Do p=1,nbins
            Do q=1,nbins
                If  (.not.found) then
                    If (p .eq. q) then
                        I(p,q) = counter
                        If (m .eq. I(p,q)) then
                            If (i_or_j .eq. 1) then
                                i_j_from_I_oa = p
                            Else if (i_or_j .eq. 2) then
                                i_j_from_I_oa = q
                            End If
                            found = .true.
                        else
                            counter = counter + 1
                        End If
                    End If
                End If
            End Do
        End Do
end function i_j_from_I_oa

subroutine ini_file_generator(param_omega_b, param_omega_cdm, param_n_s, param_A_s, param_H0, &
                              param_m_ncdm, param_tau_reio, param_N_ur, param_N_ncdm, param_deg_ncdm, &
                              param_MG_beta2,Cl_El_flag, len_flag,z_bin_centers,z_bin_widths,z_bin_bias,&
                              s_z_mag_bias,bessel,q,kmaxtau0)
    
    use fiducial
    Implicit none
    Real*8:: param_omega_b,param_omega_cdm,param_n_s,param_A_s,param_H0,param_m_ncdm,param_tau_reio
    Real*8:: param_N_ur,param_N_ncdm,param_deg_ncdm,param_MG_beta2,bessel,q,kmaxtau0
    Real*8,dimension(nbins):: z_bin_centers, z_bin_widths, z_bin_bias,s_z_mag_bias
    logical :: f1,f2,f3,f4,f5,f6,f7,fiducial_model,len_flag,Cl_El_flag
    character*16 :: string_omega_b, string_omega_cdm, string_n_s, string_A_s, string_H0, string_m_ncdm,fmt
    character*16 :: string_MG_beta2
    
    call bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)

    fmt = '(es16.10)' 
    write(string_omega_b,fmt) param_omega_b
    write(string_omega_cdm,fmt) param_omega_cdm
    write(string_n_s,fmt) param_n_s
    write(string_A_s,fmt) param_A_s
    write(string_H0,fmt) param_H0
    write(string_m_ncdm,fmt) param_m_ncdm
    write(string_MG_beta2,fmt) param_MG_beta2

    f1 = (param_omega_b == omega_b)
    f2 = (param_omega_cdm == omega_cdm)
    f3 = (param_n_s == n_s)
    f4 = (param_A_s == A_s)
    f5 = (param_H0 == H0)
    f6 = (param_m_ncdm == m_ncdm) 
    f7 = (param_MG_beta2 == MG_beta2)
    fiducial_model = ((f1 .and. f2) .and. (f3 .and. f4) .and. (f5 .and. f6) .and. f7)

    If (Cl_El_flag  .and. fiducial_model) then
        if (len_flag) then
            open(10, file='./ini_files/fiducial_euclid_galaxy.ini')
            write(10,'(a40)') 'root = ../output/fiducial_euclid_galaxy_'
            write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
        else
            open(10, file='./ini_files/fiducial_euclid_galaxy_no_lensing.ini')
            write(10,'(a51)') 'root = ../output/fiducial_euclid_galaxy_no_lensing_'
            write(10,'(a50)') 'number count contributions = density, rsd, doppler' 
        end if
    else if (fiducial_model .and. .not.Cl_El_flag) then
        if (len_flag) then
            open(10, file='./ini_files/El_fiducial_euclid_galaxy.ini')
            write(10,'(a43)') 'root = ../output/El_fiducial_euclid_galaxy_'
            write(10,'(a25)') 'number count error = 0.10'   
            write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'     
        else
            !print *, 'Error files are only computed for the fiducial model which includes lensing in our project'
            !print *, 'Turn on lensing flag '
        end if 
    else if (.not.fiducial_model .and. Cl_El_flag) then
        if ((.not.f1 .and. f2) .and. (f3 .and. f4) .and. (f5 .and. f6)) then 
            if (len_flag) then 
                open(10, file='./ini_files/Cl_euclid_galaxy_omega_b_'//trim(string_omega_b)//'_lensing.ini')
                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
                write(10,*) 'root = ../output/Cl_euclid_galaxy_omega_b_'//trim(string_omega_b)//'_lensing_'
            else 
                open(10, file='./ini_files/Cl_euclid_galaxy_omega_b_'//trim(string_omega_b)//'.ini')
                write(10,'(a50)') 'number count contributions = density, rsd, doppler'
                write(10,*) 'root = ../output/Cl_euclid_galaxy_omega_b_'//trim(string_omega_b)//'_'
            end if
        else if ((f1 .and. .not.f2) .and. (f3 .and. f4) .and. (f5 .and. f6)) then
            if (len_flag) then
                open(10, file='./ini_files/Cl_euclid_galaxy_omega_cdm_'//trim(string_omega_cdm)//'_lensing.ini')
                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
                write(10,*) 'root = ../output/Cl_euclid_galaxy_omega_cdm_'//trim(string_omega_cdm)//'_lensing_'
            else 
                open(10, file='./ini_files/Cl_euclid_galaxy_omega_cdm_'//trim(string_omega_cdm)//'.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_omega_cdm_'//trim(string_omega_cdm)//'_'
                write(10,'(a50)') 'number count contributions = density, rsd, doppler'
            end if
        else if ((f1 .and. f2) .and. (.not.f3 .and. f4) .and. (f5 .and. f6)) then 
            if (len_flag) then
                open(10, file='./ini_files/Cl_euclid_galaxy_n_s_'//trim(string_n_s)//'_lensing.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_n_s_'//trim(string_n_s)//'_lensing_'
                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
            else 
                open(10, file='./ini_files/Cl_euclid_galaxy_n_s_'//trim(string_n_s)//'.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_n_s_'//trim(string_n_s)//'_'
                write(10,'(a50)') 'number count contributions = density, rsd, doppler'
            end if
        else if ((f1 .and. f2) .and. (f3 .and. .not.f4) .and. (f5 .and. f6)) then 
            if (len_flag) then
                open(10, file='./ini_files/Cl_euclid_galaxy_A_s_'//trim(string_A_s)//'_lensing.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_A_s_'//trim(string_A_s)//'_lensing_'
                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
            else 
                open(10, file='./ini_files/Cl_euclid_galaxy_A_s_'//trim(string_A_s)//'.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_A_s_'//trim(string_A_s)//'_'
                write(10,'(a50)') 'number count contributions = density, rsd, doppler'
            end if
        else if ((f1 .and. f2) .and. (f3 .and. f4) .and. (.not.f5 .and. f6)) then 
            if (len_flag) then 
                open(10, file='./ini_files/Cl_euclid_galaxy_H0_'//trim(string_H0)//'_lensing.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_H0_'//trim(string_H0)//'_lensing_'
                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
            else
                open(10, file='./ini_files/Cl_euclid_galaxy_H0_'//trim(string_H0)//'.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_H0_'//trim(string_H0)//'_'
                write(10,'(a50)') 'number count contributions = density, rsd, doppler'
            end if
        else if ((f1 .and. f2) .and. (f3 .and. f4) .and. (f5 .and. .not.f6)) then 
            if (len_flag) then 
                open(10, file='./ini_files/Cl_euclid_galaxy_m_ncdm_'//trim(string_m_ncdm)//'_lensing.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_m_ncdm_'//trim(string_m_ncdm)//'_lensing_'
                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
            else 
                open(10, file='./ini_files/Cl_euclid_galaxy_m_ncdm_'//trim(string_m_ncdm)//'.ini')
                write(10,*) 'root = ../output/Cl_euclid_galaxy_m_ncdm_'//trim(string_m_ncdm)//'_'
                write(10,'(a50)') 'number count contributions = density, rsd, doppler'
            end if
        else 
            print *,'Varying two parameters simultaneously, that is not what we want for this project'
            stop
        end if
    else if (.not.fiducial_model .and. .not.Cl_El_flag) then 
        !print *, 'Error files are only computed for the fiducial model in our project'
    else
        print *, 'There was a problem with the ini_file_generator.'
        print *, 'Check lensing_flag and Cl_El_flag parameters. The ini file will not be created'
        stop
    End If
    
    ! Background parameters     
                                                                                                        
    write(10,'(a6, es16.10)') 'A_s = ', param_A_s  

    write(10,'(a6, es16.10)') 'n_s = ', param_n_s
 
    write(10,'(a5, es16.10)') 'H0 = ', param_H0

    write(10,'(a10, es16.10)') 'omega_b = ', param_omega_b

    write(10,'(a12, es16.10)') 'omega_cdm = ', param_omega_cdm

    write(10,'(a11, es16.10)') 'tau_reio = ', param_tau_reio

    ! Parameters for massive neutrinos                                                                                            

    write(10,'(a7, es16.10)') 'N_ur = ', param_N_ur

    write(10,'(a9, es16.10)') 'N_ncdm = ', param_N_ncdm

    write(10,'(a11, es16.10)') 'deg_ncdm = ', param_deg_ncdm

    write(10,'(a9, es16.10)') 'm_ncdm = ', param_m_ncdm

    ! Number counts in the output                                                                                            

    write(10,'(a12)') 'output = nCl'
    
    !write(10,'(a20)') 'non linear = halofit'
 
    write(10,'(a25)') 'dNdz_selection = analytic'

    write(10,'(a20)') 'selection = gaussian'

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',', z_bin_centers(3),',',&
    z_bin_centers(4),',',z_bin_centers(5)!,',',z_bin_centers(6),',',z_bin_centers(7),',',z_bin_centers(8),',',&
!    z_bin_centers(9),',',z_bin_centers(10)

    write(10,'(a18, 4(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',z_bin_widths(3),',',&
    z_bin_widths(4),',',z_bin_widths(5)!,',',z_bin_widths(6),',',z_bin_widths(7),',',z_bin_widths(8),',',&
!    z_bin_widths(9),',',z_bin_widths(10)

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',z_bin_bias(3),',',&
    z_bin_bias(4),',',z_bin_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a31, 4(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',s_z_mag_bias(2),',',&
         s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a15,i2)') 'non_diagonal = ',nbins-1

    write(10,'(a13)') 'headers = yes'

    write(10,'(a17)') 'bessel file = yes'

    write(10,'(a12,i4)') 'l_max_lss = ', lmax

    write(10,'(a8,i1)') 'l_min = ', lmin

    write(10,'(a9,f2.0)') 's_bias = ', 0.

    write(10,'(a13)') 'format = camb'

    write(10,'(a17)') 'gauge = newtonian'

    ! PRECISION PARAMETERS

    write(10,'(a40, f6.0)') 'l_switch_limber_for_cl_density_over_z = ', real(l_switch_limber_for_cl_density_over_z)

    write(10,'(a28, f5.2)') 'selection_sampling_bessel = ', real(bessel)

    write(10,'(a12, f5.1)') 'q_linstep = ', real(q)

    write(10,'(a24, f5.2)') 'k_max_tau0_over_l_max = ', real(kmaxtau0)

    close(10)

end subroutine ini_file_generator

subroutine write_ini_file_for_fisher(parameter_name, parameter_value, lensing_flag, Cl_flag,bessel,q,kmaxtau0)
    use fiducial
    use arrays
    Implicit none
    Real*8:: parameter_value,bessel,q,kmaxtau0
    Real*8,dimension(nbins):: z_bin_centers, z_bin_widths, z_bin_bias,s_z_mag_bias
    logical :: lensing_flag,Cl_flag,fid1,fid2,fid3,fid4,fid5,fid6,fid7,fiducial_flag,bestfit_flag
    character(len=*) :: parameter_name
    character(len=*),parameter :: fmt = '(es16.10)' 
    character*16 :: string_par_value

    If (fisher_analysis_at_bestfit) then

       fid1 = parameter_value .eq. bestfit(1) ! omega_b

       fid2 = parameter_value .eq. bestfit(2) ! omega_cdm

       fid3 = parameter_value .eq. bestfit(3) ! n_s

       fid4 = parameter_value .eq. bestfit(4) ! A_s

       fid5 = parameter_value .eq. bestfit(5) ! H0

       fid6 = parameter_value .eq. bestfit(6) ! m_ncdm

       fid7 = parameter_value .eq. bestfit(7) ! MG_beta2

       bestfit_flag = ((fid1 .or. fid2) .or. (fid3 .or. fid4)) .or. ((fid5 .or. fid6) .or. fid7)
     
       call bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)

       write(string_par_value,fmt) parameter_value

       If (lensing_flag) then

          If (Cl_flag) then

             If (bestfit_flag) then

                open(10, file='./ini_files/Cl_bestfit_lensing.ini')

                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

                write(10,*) 'root = ../data/Cl_bestfit_lensing_'

             Else 

                print *, 'INI FILE FOR BESTFIT WITH LENSING NOT IMPLEMENTED YET'

                stop

!                open(10, file='./ini_files/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing.ini')

!                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

!                write(10,*) 'root = ../data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing_'

             End If

          Else

             print *, 'NO INI FILE FOR ERROR WITH LENSING AT BEST FIT IMPLEMENTED'

             stop

!             open(10, file='./ini_files/El.ini')

!             write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

!             write(10,*) 'root = ../data/El_'

!             write(10,'(a25)') 'number count error = 0.10'

          End if

       Else 

          If (bestfit_flag) then

             open(10, file='./ini_files/Cl_bestfit_no_lensing.ini')

             write(10,'(a50)') 'number count contributions = density, rsd, doppler'

             write(10,*) 'root = ../data/Cl_bestfit_no_lensing_'

          Else

             print *, 'MUST IMPLEMENT INI FILE AROUND BEST FIT'

             stop

!             open(10, file='./ini_files/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing.ini')

!             write(10,'(a50)') 'number count contributions = density, rsd, doppler'

!             write(10,*) 'root = ../data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing_'

          End If

       End if

       ! Background parameters and anisotropic stress

       If (parameter_name .ne. param_name_omega_b) then

          write(10,'(a10, es16.10)') 'omega_b = ', bestfit(1) 

       Else

          write(10,'(a10, es16.10)') 'omega_b = ', parameter_value

       End If

       If (parameter_name .ne. param_name_omega_cdm) then

          write(10,'(a12, es16.10)') 'omega_cdm = ', bestfit(2)

       Else

          write(10,'(a12, es16.10)') 'omega_cdm = ', parameter_value

       End If

       If (parameter_name .ne. param_name_n_s) then

          write(10,'(a6, es16.10)') 'n_s = ', bestfit(3)

       else

          write(10,'(a6, es16.10)') 'n_s = ', parameter_value

       End If

       If (parameter_name .ne. param_name_A_s) then 
 
          write(10,'(a6, es16.10)') 'A_s = ', bestfit(4) 

       else

          write(10,'(a6, es16.10)') 'A_s = ', parameter_value  

       End If

       If (parameter_name .ne. param_name_H0) then

          write(10,'(a5, es16.10)') 'H0 = ', bestfit(5)

       Else

          write(10,'(a5, es16.10)') 'H0 = ', parameter_value

       End If

       If (parameter_name .ne. param_name_m_ncdm) then

          write(10,'(a9, es16.10)') 'm_ncdm = ', bestfit(6)

       Else

          write(10,'(a9, es16.10)') 'm_ncdm = ', parameter_value

       End If

       If (parameter_name .ne. param_name_MG_beta2) then

          write(10,'(a11, es16.10)') 'MG_beta2 = ', bestfit(7)

       Else

          write(10,'(a11, es16.10)') 'MG_beta2 = ', parameter_value
 
       End If

    Else

       fid1 = parameter_value .eq. omega_b

       fid2 = parameter_value .eq. omega_cdm

       fid3 = parameter_value .eq. n_s

       fid4 = parameter_value .eq. A_s

       fid5 = parameter_value .eq. H0

       fid6 = parameter_value .eq. m_ncdm

       fid7 = parameter_value .eq. MG_beta2

       fiducial_flag = ((fid1 .or. fid2) .or. (fid3 .or. fid4)) .or. ((fid5 .or. fid6) .or. fid7)
     
       call bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)

       write(string_par_value,fmt) parameter_value

       If (lensing_flag) then

          If (Cl_flag) then

             If (fiducial_flag) then

                open(10, file='./ini_files/Cl_fiducial_lensing.ini')

                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

                write(10,*) 'root = ../data/Cl_fiducial_lensing_'

             Else 

                open(10, file='./ini_files/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing.ini')

                write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

                write(10,*) 'root = ../data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing_'

             End If

          Else

             open(10, file='./ini_files/El.ini')

             write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

             write(10,*) 'root = ../data/El_'

             write(10,'(a25)') 'number count error = 0.10'

          End if

       Else 

          If (fiducial_flag) then

             open(10, file='./ini_files/Cl_fiducial_no_lensing.ini')

             write(10,'(a50)') 'number count contributions = density, rsd, doppler'

             write(10,*) 'root = ../data/Cl_fiducial_no_lensing_'

          Else

             open(10, file='./ini_files/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing.ini')

             write(10,'(a50)') 'number count contributions = density, rsd, doppler'

             write(10,*) 'root = ../data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing_'

          End If

       End if

       ! Background parameters and anisotropic stress

       If (parameter_name .ne. param_name_A_s) then 
 
          write(10,'(a6, es16.10)') 'A_s = ', A_s  

       else

          write(10,'(a6, es16.10)') 'A_s = ', parameter_value  

       End If

       If (parameter_name .ne. param_name_n_s) then

          write(10,'(a6, es16.10)') 'n_s = ', n_s

       else

          write(10,'(a6, es16.10)') 'n_s = ', parameter_value

       End If
 
       If (parameter_name .ne. param_name_H0) then

          write(10,'(a5, es16.10)') 'H0 = ', H0

       Else

          write(10,'(a5, es16.10)') 'H0 = ', parameter_value

       End If

       If (parameter_name .ne. param_name_omega_b) then

          write(10,'(a10, es16.10)') 'omega_b = ', omega_b

       Else

          write(10,'(a10, es16.10)') 'omega_b = ', parameter_value

       End If

       If (parameter_name .ne. param_name_omega_cdm) then

          write(10,'(a12, es16.10)') 'omega_cdm = ', omega_cdm

       Else

          write(10,'(a12, es16.10)') 'omega_cdm = ', parameter_value

       End If

       If (parameter_name .ne. param_name_MG_beta2) then

          write(10,'(a11, es16.10)') 'MG_beta2 = ', MG_beta2

       Else

          write(10,'(a11, es16.10)') 'MG_beta2 = ', parameter_value
 
       End If

       If (parameter_name .ne. param_name_m_ncdm) then

          write(10,'(a9, es16.10)') 'm_ncdm = ', m_ncdm

       Else

          write(10,'(a9, es16.10)') 'm_ncdm = ', parameter_value

       End If

    End If
    
    write(10,'(a11, es16.10)') 'tau_reio = ', tau

    write(10,'(a7, f5.3)') 'N_ur = ', real(N_ur)

    write(10,'(a9, f5.3)') 'N_ncdm = ', real(N_ncdm)

    write(10,'(a11, f5.3)') 'deg_ncdm = ', real(deg_ncdm)

    ! Number counts in the output                                                                                            

    write(10,'(a12)') 'output = nCl'
    
!    write(10,'(a20)') 'non linear = halofit'
 
    write(10,'(a32)') 'dNdz_selection = analytic_euclid'

    write(10,'(a17)') 'dNdz_evolution = '

    write(10,'(a20)') 'selection = gaussian'

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',', z_bin_centers(3),',',&
    z_bin_centers(4),',',z_bin_centers(5)!,',',z_bin_centers(6),',',z_bin_centers(7),',',z_bin_centers(8),',',&
    !z_bin_centers(9),',',z_bin_centers(10)

    write(10,'(a18, 4(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',z_bin_widths(3),',',&
    z_bin_widths(4),',',z_bin_widths(5)!,',',z_bin_widths(6),',',z_bin_widths(7),',',z_bin_widths(8),',',&
!    z_bin_widths(9),',',z_bin_widths(10)

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',z_bin_bias(3),',',&
    z_bin_bias(4),',',z_bin_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a31, 4(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',s_z_mag_bias(2),',',&
         s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a15,i2)') 'non_diagonal = ',nbins-1

    write(10,'(a13)') 'headers = yes'

    write(10,'(a17)') 'bessel file = yes'

    write(10,'(a12,i4)') 'l_max_lss = ', lmax_class

    write(10,'(a8,i1)') 'l_min = ', lmin

    write(10,'(a27,f2.0)') 'selection_magnitude_bias = ', 0.

    write(10,'(a14)') 'format = class'

    write(10,'(a17)') 'gauge = newtonian'

    ! PRECISION PARAMETERS

    write(10,'(a40, f6.0)') 'l_switch_limber_for_cl_density_over_z = ', real(l_switch_limber_for_cl_density_over_z)

    write(10,'(a28, f5.2)') 'selection_sampling_bessel = ', real(bessel)

    write(10,'(a12, f5.1)') 'q_linstep = ', real(q)

    write(10,'(a24, f5.2)') 'k_max_tau0_over_l_max = ', real(kmaxtau0)

    close(10)

end subroutine write_ini_file_for_fisher

subroutine write_ini_file_mcmc(param_omega_b, param_omega_cdm, param_n_s, param_A_s, param_H0, &
                          param_m_ncdm, param_MG_beta2,param_tau_reio, param_N_ur, param_N_ncdm,&
                           param_deg_ncdm, len_flag,bessel,q,kmaxtau0,job)
    
    use fiducial
    Implicit none
    Real*8:: param_omega_b,param_omega_cdm,param_n_s,param_A_s,param_H0,param_m_ncdm,param_tau_reio
    Real*8:: param_N_ur,param_N_ncdm,param_deg_ncdm,param_MG_beta2,bessel,q,kmaxtau0
    Real*8,dimension(nbins):: z_bin_centers, z_bin_widths, z_bin_bias,s_z_mag_bias
    logical :: len_flag
    character*16 :: string_omega_b, string_omega_cdm, string_n_s, string_A_s, string_H0, string_m_ncdm,fmt
    character*16 :: string_MG_beta2
    Character(len=10) :: job
        
    call bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)

    fmt = '(es16.10)' 
    write(string_omega_b,fmt) param_omega_b
    write(string_omega_cdm,fmt) param_omega_cdm
    write(string_n_s,fmt) param_n_s
    write(string_A_s,fmt) param_A_s
    write(string_H0,fmt) param_H0
    write(string_m_ncdm,fmt) param_m_ncdm
    write(string_MG_beta2,fmt) param_MG_beta2

    If (len_flag) then 
        open(10, file='./ini_files/current_euclid_galaxy_cl_lensing_'//trim(job)//'.ini')
        write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
        write(10,*) 'root = ../output/current_euclid_galaxy_lensing_'//trim(job)//'_'
    else 
        open(10, file='./ini_files/current_euclid_galaxy_cl_'//trim(job)//'.ini')
        write(10,'(a50)') 'number count contributions = density, rsd, doppler'
        write(10,*) 'root = ../output/current_euclid_galaxy_cl_'//trim(job)//'_'
    End if
    
    ! Background parameters and anisotropic stress
                                                                                                        
    write(10,'(a6, es16.10)') 'A_s = ', param_A_s  

    write(10,'(a6, es16.10)') 'n_s = ', param_n_s
 
    write(10,'(a5, es16.10)') 'H0 = ', param_H0

    write(10,'(a10, es16.10)') 'omega_b = ', param_omega_b

    write(10,'(a12, es16.10)') 'omega_cdm = ', param_omega_cdm

    write(10,'(a11, es16.10)') 'tau_reio = ', param_tau_reio

    write(10,'(a11, es16.10)') 'MG_beta2 = ', param_MG_beta2

    ! Parameters for massive neutrinos                                                                                            

    write(10,'(a7, f5.3)') 'N_ur = ', real(param_N_ur)

    write(10,'(a9, f5.3)') 'N_ncdm = ', real(param_N_ncdm)

    write(10,'(a11, f5.3)') 'deg_ncdm = ', real(param_deg_ncdm)

    write(10,'(a9, es16.10)') 'm_ncdm = ', param_m_ncdm

    ! Number counts in the output                                                                                            

    write(10,'(a12)') 'output = nCl'
    
    !write(10,'(a20)') 'non linear = halofit'
 
    write(10,'(a32)') 'dNdz_selection = analytic_euclid'

    write(10,'(a17)') 'dNdz_evolution = '

    write(10,'(a20)') 'selection = gaussian'

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',', z_bin_centers(3),',',&
    z_bin_centers(4),',',z_bin_centers(5)!,',',z_bin_centers(6),',',z_bin_centers(7),',',z_bin_centers(8),',',&
!    z_bin_centers(9),',',z_bin_centers(10)

    write(10,'(a18, 4(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',z_bin_widths(3),',',&
    z_bin_widths(4),',',z_bin_widths(5)!,',',z_bin_widths(6),',',z_bin_widths(7),',',z_bin_widths(8),',',&
 !   z_bin_widths(9),',',z_bin_widths(10)

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',z_bin_bias(3),',',&
    z_bin_bias(4),',',z_bin_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
  !  z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a31, 4(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',s_z_mag_bias(2),',',&
         s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a15,i2)') 'non_diagonal = ',nbins-1

    write(10,'(a13)') 'headers = yes'

    write(10,'(a17)') 'bessel file = yes'

    write(10,'(a12,i4)') 'l_max_lss = ', lmax_class

    write(10,'(a8,i1)') 'l_min = ', lmin

    write(10,'(a27,f2.0)') 'selection_magnitude_bias = ', 0.

    write(10,'(a14)') 'format = class'

    write(10,'(a17)') 'gauge = newtonian'

    ! PRECISION PARAMETERS

    write(10,'(a40, f6.0)') 'l_switch_limber_for_cl_density_over_z = ', real(l_switch_limber_for_cl_density_over_z)

    write(10,'(a28, f5.2)') 'selection_sampling_bessel = ', real(bessel)

    write(10,'(a12, f5.1)') 'q_linstep = ', real(q)

    write(10,'(a24, f5.2)') 'k_max_tau0_over_l_max = ', real(kmaxtau0)

    close(10)

end subroutine write_ini_file_mcmc

subroutine write_ini_file(param_omega_b, param_omega_cdm, param_n_s, param_A_s, param_H0, &
                          param_m_ncdm, param_MG_beta2,param_tau_reio, param_N_ur, param_N_ncdm,&
                           param_deg_ncdm, len_flag,bessel,q,kmaxtau0)
    
    use fiducial
    Implicit none
    Real*8:: param_omega_b,param_omega_cdm,param_n_s,param_A_s,param_H0,param_m_ncdm,param_tau_reio
    Real*8:: param_N_ur,param_N_ncdm,param_deg_ncdm,param_MG_beta2,bessel,q,kmaxtau0
    Real*8,dimension(nbins):: z_bin_centers, z_bin_widths, z_bin_bias,s_z_mag_bias
    logical :: len_flag
    character*16 :: string_omega_b, string_omega_cdm, string_n_s, string_A_s, string_H0, string_m_ncdm,fmt
    character*16 :: string_MG_beta2
        
    call bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)

    fmt = '(es16.10)' 
    write(string_omega_b,fmt) param_omega_b
    write(string_omega_cdm,fmt) param_omega_cdm
    write(string_n_s,fmt) param_n_s
    write(string_A_s,fmt) param_A_s
    write(string_H0,fmt) param_H0
    write(string_m_ncdm,fmt) param_m_ncdm
    write(string_MG_beta2,fmt) param_MG_beta2

    If (len_flag) then 
        open(10, file='./ini_files/current_euclid_galaxy_cl_lensing.ini')
        write(10,'(a59)') 'number count contributions = density, rsd, lensing, doppler'
        write(10,*) 'root = ../output/current_euclid_galaxy_lensing_'
    else 
        open(10, file='./ini_files/current_euclid_galaxy_cl_.ini')
        write(10,'(a50)') 'number count contributions = density, rsd, doppler'
        write(10,*) 'root = ../output/current_euclid_galaxy_'
    End if
    
    ! Background parameters and anisotropic stress
                                                                                                        
    write(10,'(a6, es16.10)') 'A_s = ', param_A_s  

    write(10,'(a6, es16.10)') 'n_s = ', param_n_s
 
    write(10,'(a5, es16.10)') 'H0 = ', param_H0

    write(10,'(a10, es16.10)') 'omega_b = ', param_omega_b

    write(10,'(a12, es16.10)') 'omega_cdm = ', param_omega_cdm

    write(10,'(a11, es16.10)') 'tau_reio = ', param_tau_reio

    write(10,'(a11, es16.10)') 'MG_beta2 = ', param_MG_beta2

    ! Parameters for massive neutrinos                                                                                            

    write(10,'(a7, f5.3)') 'N_ur = ', real(param_N_ur)

    write(10,'(a9, f5.3)') 'N_ncdm = ', real(param_N_ncdm)

    write(10,'(a11, f5.3)') 'deg_ncdm = ', real(param_deg_ncdm)

    write(10,'(a9, es16.10)') 'm_ncdm = ', param_m_ncdm

    ! Number counts in the output                                                                                            

    write(10,'(a12)') 'output = nCl'
    
    !write(10,'(a20)') 'non linear = halofit'
 
    write(10,'(a32)') 'dNdz_selection = analytic_euclid'

    write(10,'(a17)') 'dNdz_evolution = '

    write(10,'(a20)') 'selection = gaussian'

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',', z_bin_centers(3),',',&
    z_bin_centers(4),',',z_bin_centers(5)!,',',z_bin_centers(6),',',z_bin_centers(7),',',z_bin_centers(8),',',&
!    z_bin_centers(9),',',z_bin_centers(10)

    write(10,'(a18, 4(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',z_bin_widths(3),',',&
    z_bin_widths(4),',',z_bin_widths(5)!,',',z_bin_widths(6),',',z_bin_widths(7),',',z_bin_widths(8),',',&
 !   z_bin_widths(9),',',z_bin_widths(10)

    write(10,'(a17, 4(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',z_bin_bias(3),',',&
    z_bin_bias(4),',',z_bin_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
  !  z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a31, 4(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',s_z_mag_bias(2),',',&
         s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5)!,',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
!    z_bin_bias(9),',',z_bin_bias(10)

    write(10,'(a15,i2)') 'non_diagonal = ',nbins-1

    write(10,'(a13)') 'headers = yes'

    write(10,'(a17)') 'bessel file = yes'

    write(10,'(a12,i4)') 'l_max_lss = ', lmax_class

    write(10,'(a8,i1)') 'l_min = ', lmin

    write(10,'(a27,f2.0)') 'selection_magnitude_bias = ', 0.

    write(10,'(a14)') 'format = class'

    write(10,'(a17)') 'gauge = newtonian'

    ! PRECISION PARAMETERS

    write(10,'(a40, f6.0)') 'l_switch_limber_for_cl_density_over_z = ', real(l_switch_limber_for_cl_density_over_z)

    write(10,'(a28, f5.2)') 'selection_sampling_bessel = ', real(bessel)

    write(10,'(a12, f5.1)') 'q_linstep = ', real(q)

    write(10,'(a24, f5.2)') 'k_max_tau0_over_l_max = ', real(kmaxtau0)

    close(10)

end subroutine write_ini_file

subroutine fill_parameters_array(p)
    use arrays
    use fiducial
    Implicit none
    Integer*4 p,m,index
    logical :: lensing_flag

    ! Fill array of cosmological parameters to be used for first step of Fisher analysis (Derivatives) 

    Do m=-p,p    
        param_omega_b(m+p) = omega_b + m*sigma_omega_b   
        param_omega_cdm(m+p) = omega_cdm + m*sigma_omega_cdm
        param_n_s(m+p) = n_s + m*sigma_n_s
        param_A_s(m+p) = A_s + m*sigma_A_s
        param_H0(m+p) = H0 + m*sigma_H0
        param_m_ncdm(m+p) = m_ncdm + m*sigma_m_ncdm
        !param_MG_beta2(m+p) = MG_beta2 + m*sigma_MG_beta2
    End Do

    If (compute_data_fisher_analysis) then
        ! Write ini files for Cl's with/without lensing

       If (fisher_analysis_at_bestfit) then

          call read_bestfit_mcmc(bestfit)

          Do m=-p,p    

             param_omega_b(m+p) = bestfit(1) + m*sigma_omega_b   

             param_omega_cdm(m+p) = bestfit(2) + m*sigma_omega_cdm

             param_n_s(m+p) = bestfit(3) + m*sigma_n_s

             param_A_s(m+p) = bestfit(4) + m*sigma_A_s

             param_H0(m+p) = bestfit(5) + m*sigma_H0

             param_m_ncdm(m+p) = bestfit(6) + m*sigma_m_ncdm

         !    param_MG_beta2(m+p) = bestfit(7) + m*sigma_MG_beta2

          End Do

          call write_ini_file_for_fisher('omega_b',bestfit(1),.false.,.true.,&
               selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)    

       Else

          Do index=1,2

             If (index .eq. 1) then

                lensing_flag = .true.     

             Else

                lensing_flag = .false.     

             End If

             Do m=0,2*p

                call write_ini_file_for_fisher('omega_b',param_omega_b(m),lensing_flag,.true.,&
                     selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)    

                call write_ini_file_for_fisher('omega_cdm',param_omega_cdm(m),lensing_flag,.true.,&
                     selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

                call write_ini_file_for_fisher('n_s',param_n_s(m),lensing_flag,.true.,&
                     selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

                call write_ini_file_for_fisher('A_s',param_A_s(m),lensing_flag,.true.,&
                     selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

                call write_ini_file_for_fisher('H0',param_H0(m),lensing_flag,.true.,&
                     selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

                call write_ini_file_for_fisher('m_ncdm',param_m_ncdm(m),lensing_flag,.true.,&
                     selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

           !     call write_ini_file_for_fisher('MG_beta2',param_MG_beta2(m),lensing_flag,.true.,&
            !         selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

             End Do

          End Do

          ! Write ini file for error file including lensing and only for fiducial model

          call write_ini_file_for_fisher('H0',H0,.true.,.false.,&
               selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

       End If

    End If

end subroutine fill_parameters_array

subroutine compute_data_for_fisher_analysis(p)
    use arrays
    use fiducial
    Implicit none
    Integer*4 p,m,index
    logical :: lensing_flag

    Do index=1,2

        If (index .eq. 1) then

            lensing_flag = .true.     

        Else

            lensing_flag = .false.     

        End If

        Do m=0,2*p

            call run_class('omega_b',param_omega_b(m),lensing_flag,.true.)

            call run_class('omega_cdm',param_omega_cdm(m),lensing_flag,.true.)

            call run_class('n_s',param_n_s(m),lensing_flag,.true.)

            call run_class('A_s',param_A_s(m),lensing_flag,.true.)

            call run_class('H0',param_H0(m),lensing_flag,.true.)

            call run_class('m_ncdm',param_m_ncdm(m),lensing_flag,.true.)

            !call run_class('MG_beta2',param_MG_beta2(m),lensing_flag,.true.)

        End Do

    End Do

    call run_class('H0',H0,.true.,.false.)

end subroutine compute_data_for_fisher_analysis

subroutine write_sh_file(name_ini_file)
    Implicit none
    character(len=*) :: name_ini_file

    open(12,file='./class_montanari-lensing/'//trim(name_ini_file)//'.sh')

    write(12,'(a9)') '#!/bin/sh'
    write(12,'(a26)') '#SBATCH --cpus-per-task=12'
    write(12,'(a24)') '#SBATCH --job-name=CLASS'
    write(12,'(a18)') '#SBATCH --ntasks=1'
    write(12,'(a25)') '#SBATCH --time=7-00:00:00'
    write(12,'(a43)') '#SBATCH --mail-user=wilmar.cardona@unige.ch'
    write(12,'(a23)') '#SBATCH --mail-type=ALL'
    write(12,'(a23)') '#SBATCH --partition=dpt'
    write(12,'(a25)') '#SBATCH --clusters=baobab'
    write(12,'(a29)') '#SBATCH --output=slurm-%J.out'
    write(12,*) 
    write(12,*)'srun ./class ../ini_files/'//trim(name_ini_file)//'.ini'

    close(12)
end subroutine write_sh_file

subroutine run_class(parameter_name,parameter_value,lensing_flag,Cl_flag)
    use arrays
    use fiducial
    Implicit none
    Real*8 ::parameter_value
    character(len=*) :: parameter_name
    character*16 :: string_par_value
    logical :: exist,lensing_flag,Cl_flag,fid1,fid2,fid3,fid4,fid5,fid6,fiducial_flag,bestfit_flag,fid7
    character(len=*),parameter :: fmt = '(es16.10)'

    If (fisher_analysis_at_bestfit) then

       fid1 = parameter_value .eq. bestfit(1) ! omega_b

       fid2 = parameter_value .eq. bestfit(2) ! omega_cdm

       fid3 = parameter_value .eq. bestfit(3) ! n_s

       fid4 = parameter_value .eq. bestfit(4) ! A_s

       fid5 = parameter_value .eq. bestfit(5) ! H0

       fid6 = parameter_value .eq. bestfit(6) ! m_ncdm

       !fid7 = parameter_value .eq. bestfit(7) ! MG_beta2

       bestfit_flag = ((fid1 .or. fid2) .or. (fid3 .or. fid4)) .or. (fid5 .or. fid6)! .or. fid7)
     
       write(string_par_value,fmt) parameter_value

       If (lensing_flag) then

          If (Cl_flag) then

             If (bestfit_flag) then
 
                !inquire(file='./data/Cl_bestfit_lensing_cl.dat',exist=exist)

                !If (.not.exist) then

                !   call write_sh_file('Cl_bestfit_lensing')

                !   call system('cd class_montanari-lensing ; sbatch Cl_bestfit_lensing.sh')

                !End If

                print *,'CLS NOT COMPUTED FOR BESTFIT+LENSING FOR THE MOMENT'

                continue 

             Else 

                print *,'FISHER AROUND BEST FIT NOT IMPLEMENTED YET'

                continue

!                inquire(file='./data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing_cl.dat',exist=exist)

!                If (.not.exist) then

!                   call write_sh_file('Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing')

!                   call system('cd class_montanari-lensing ; sbatch Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//&
!                        '_lensing.sh')

!                End If

             End If

          Else

             print *, 'ERROR FILE AT BEST FIT NOT IMPLEMENTED YET'

             continue

!             inquire(file='./data/El_cl.dat',exist=exist)

!             If (.not.exist) then

!                call write_sh_file('El')

!                call system('cd class_montanari-lensing ; sbatch El.sh')

!             End If

          End if

       Else 

          If (bestfit_flag) then

             inquire(file='./data/Cl_bestfit_no_lensing_cl.dat',exist=exist)

             If (.not.exist) then

                call write_sh_file('Cl_bestfit_no_lensing')

                call system('cd class_montanari-lensing ; sbatch Cl_bestfit_no_lensing.sh')

             End If

          Else

             print *,'FISHER ANALYSIS AROUND BEST FIT NOT IMPLEMENTED YET WITHOUT LENSING'

             continue

!             inquire(file='./data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing_cl.dat',exist=exist)

!             If (.not.exist) then

!                call write_sh_file('Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing')

!                call system('cd class_montanari-lensing ; sbatch Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//&
!                     '_no_lensing.sh')

!             End If

          End If

       End if

    Else

       fid1 = parameter_value .eq. omega_b

       fid2 = parameter_value .eq. omega_cdm

       fid3 = parameter_value .eq. n_s

       fid4 = parameter_value .eq. A_s

       fid5 = parameter_value .eq. H0

       fid6 = parameter_value .eq. m_ncdm

       fid7 = parameter_value .eq. MG_beta2

       fiducial_flag = ((fid1 .or. fid2) .or. (fid3 .or. fid4)) .or. ((fid5 .or. fid6) .or. fid7)
     
       write(string_par_value,fmt) parameter_value

       If (lensing_flag) then

          If (Cl_flag) then

             If (fiducial_flag) then
 
                inquire(file='./data/Cl_fiducial_lensing_cl.dat',exist=exist)

                If (.not.exist) then

                   call write_sh_file('Cl_fiducial_lensing')

                   call system('cd class_montanari-lensing ; sbatch Cl_fiducial_lensing.sh')

                End If

             Else 

                inquire(file='./data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing_cl.dat',exist=exist)

                If (.not.exist) then

                   call write_sh_file('Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_lensing')

                   call system('cd class_montanari-lensing ; sbatch Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//&
                        '_lensing.sh')

                End If

             End If

          Else

             inquire(file='./data/El_cl.dat',exist=exist)

             If (.not.exist) then

                call write_sh_file('El')

                call system('cd class_montanari-lensing ; sbatch El.sh')

             End If

          End if

       Else 

          If (fiducial_flag) then

             inquire(file='./data/Cl_fiducial_no_lensing_cl.dat',exist=exist)

             If (.not.exist) then

                call write_sh_file('Cl_fiducial_no_lensing')

                call system('cd class_montanari-lensing ; sbatch Cl_fiducial_no_lensing.sh')

             End If

          Else

             inquire(file='./data/Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing_cl.dat',exist=exist)

             If (.not.exist) then

                call write_sh_file('Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//'_no_lensing')

                call system('cd class_montanari-lensing ; sbatch Cl_'//trim(parameter_name)//'_'//trim(string_par_value)//&
                     '_no_lensing.sh')

             End If

          End If

       End if

    End If

end subroutine run_class

subroutine run_current_model(len_flag)
    use arrays
    use fiducial
    Implicit none
    logical :: exist,len_flag
    
    If (len_flag) then
        inquire(file='./output/current_euclid_galaxy_lensing_cl.dat',exist=exist)
        If (.not.exist) then
            call system ('cd class_montanari-lensing; ./class '//trim(' ')//&
            '../ini_files/current_euclid_galaxy_cl_lensing.ini')
        End If
    else
        inquire(file='./output/current_euclid_galaxy_cl.dat',exist=exist)
        If (.not.exist) then
            call system ('cd class_montanari-lensing; ./class '//trim(' ')//&
            '../ini_files/current_euclid_galaxy_cl_.ini')
        End If
    End if

end subroutine run_current_model

subroutine run_current_model_mcmc(len_flag,job)
    use arrays
    use fiducial
    Implicit none
    logical :: exist,len_flag
    Character(len=10) :: job
    
    If (len_flag) then
        inquire(file='./output/current_euclid_galaxy_lensing_cl_'//trim(job)//'_cl.dat',exist=exist)
        If (.not.exist) then
            call system ('cd class_montanari-lensing; ./class '//trim(' ')//&
            '../ini_files/current_euclid_galaxy_cl_lensing_'//trim(job)//'.ini')
        End If
    else
        inquire(file='./output/current_euclid_galaxy_cl_'//trim(job)//'_cl.dat',exist=exist)
        If (.not.exist) then
            call system ('cd class_montanari-lensing; ./class '//trim(' ')//&
            '../ini_files/current_euclid_galaxy_cl_'//trim(job)//'.ini')
        End If
    End if

end subroutine run_current_model_mcmc

subroutine compute_derivatives()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: m,p,l,i,j
    character(len=15),dimension(7) :: params
    character*16 :: string_omega_b, string_omega_cdm, string_n_s, string_A_s, string_H0, string_m_ncdm, string_MG_beta2,fmt1
    Integer*4,dimension(8) :: indexp
    Real*8 :: h
    indexp(1) = 0
    indexp(2) = 1
    indexp(3) = 3
    indexp(4) = 4
    indexp(5) = 0
    indexp(6) = 1
    indexp(7) = 3
    indexp(8) = 4
    fmt1 = '(es16.10)'
    params(1) = 'omega_b'
    params(2) = 'omega_cdm'
    params(3) = 'n_s'
    params(4) = 'A_s'
    params(5) = 'H0'
    params(6) = 'm_ncdm'
    params(7) = 'MG_beta2'

    Do p=1,7
        Do m=1,8
            If (p .eq. 1) then 
                write(string_omega_b,fmt1) param_omega_b(indexp(m))
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_omega_b,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_omega_b,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_omega_b,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_omega_b,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_omega_b,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_omega_b,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_omega_b,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_omega_b,.false.,.false.,.true.)
                End If
            End If

            If (p .eq. 2) then 
                write(string_omega_cdm,fmt1) param_omega_cdm(indexp(m))
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_omega_cdm,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_omega_cdm,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_omega_cdm,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_omega_cdm,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_omega_cdm,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_omega_cdm,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_omega_cdm,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_omega_cdm,.false.,.false.,.true.)
                End If
            End If

            If (p .eq. 3) then 
                write(string_n_s,fmt1) param_n_s(indexp(m))
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_n_s,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_n_s,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_n_s,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_n_s,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_n_s,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_n_s,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_n_s,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_n_s,.false.,.false.,.true.)
                End If
            End If

            If (p .eq. 4) then 
                write(string_A_s,fmt1) param_A_s(indexp(m))
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_A_s,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_A_s,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_A_s,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_A_s,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_A_s,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_A_s,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_A_s,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_A_s,.false.,.false.,.true.)
                End If
            End If

            If (p .eq. 5) then 
                write(string_H0,fmt1) param_H0(indexp(m))
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_H0,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_H0,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_H0,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_H0,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_H0,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_H0,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_H0,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_H0,.false.,.false.,.true.)
                End If
            End If
                
            If (p .eq. 6) then 
                write(string_m_ncdm,fmt1) param_m_ncdm(indexp(m)) 
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_m_ncdm,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_m_ncdm,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_m_ncdm,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_m_ncdm,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_m_ncdm,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_m_ncdm,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_m_ncdm,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_m_ncdm,.false.,.false.,.true.)
                End If
            End If

            If (p .eq. 7) then 
                write(string_MG_beta2,fmt1) param_MG_beta2(indexp(m)) 
                If (m .eq. 1) then
                    call read_data(Cl_1,m,params(p),string_MG_beta2,.true.,.false.,.true.)
                else if (m .eq. 2) then
                    call read_data(Cl_2,m,params(p),string_MG_beta2,.true.,.false.,.true.)
                else if (m .eq. 3) then
                    call read_data(Cl_3,m,params(p),string_MG_beta2,.true.,.false.,.true.)
                else if (m .eq. 4) then
                    call read_data(Cl_4,m,params(p),string_MG_beta2,.true.,.false.,.true.)
                else if (m .eq. 5) then
                    call read_data(Cl_5,m,params(p),string_MG_beta2,.false.,.false.,.true.)
                else if (m .eq. 6) then
                    call read_data(Cl_6,m,params(p),string_MG_beta2,.false.,.false.,.true.)
                else if (m .eq. 7) then
                    call read_data(Cl_7,m,params(p),string_MG_beta2,.false.,.false.,.true.)
                else if (m .eq. 8) then
                    call read_data(Cl_8,m,params(p),string_MG_beta2,.false.,.false.,.true.)
                End If
            End If

        End Do

        If (p .eq. 1) then
            h = sigma_omega_b
        else if (p .eq. 2) then
            h = sigma_omega_cdm
        else if (p .eq. 3) then
            h = sigma_n_s
        else if (p .eq. 4) then
            h = sigma_A_s
        else if (p .eq. 5) then
            h = sigma_H0
        else if (p .eq. 6) then
            h = sigma_m_ncdm
        else if (p .eq. 7) then
            h = sigma_MG_beta2
        End If

        Do l=lmin,lmax
            Do i=1,nbins
                Do j=1,nbins
                    dCl(l,i,j) = 2.d0*Pi*(-Cl_4(l,i,j) + 8.d0*Cl_3(l,i,j) - 8.d0*Cl_2(l,i,j) + Cl_1(l,i,j))/12.d0/h&
                    /real(l)/(real(l)+1.d0)
                    dCl_nl(l,i,j) = 2.d0*Pi*(-Cl_8(l,i,j) + 8.d0*Cl_7(l,i,j) - 8.d0*Cl_6(l,i,j) + Cl_5(l,i,j))/12.d0/h&
                    /real(l)/(real(l)+1.d0)
                End Do
            End Do
        End Do
        call write_data(dCl,p,params(p),.true.)
        call write_data(dCl_nl,p,params(p),.false.)
    End Do
end subroutine compute_derivatives

subroutine read_covariance_matrix_mcmc(matrix1)
    use fiducial
    Implicit none
    Real*8,dimension(number_of_parameters,number_of_parameters) :: matrix,matrix1
    Integer*4 :: index1,INFO
    Integer*4,parameter :: LWORK = max(1,3*number_of_parameters-1)
    Real*8,dimension(max(1,LWORK)) :: WORK
    Real*8,dimension(number_of_parameters) :: W
    Character*1,parameter :: JOBZ = 'N'
    Character*1,parameter :: UPLO = 'U'
    Logical :: pos_def,exist 
 
    inquire(file='./output/chains/covariance_matrix.txt',exist=exist)

    If (exist) then

       open(12,file='./output/chains/covariance_matrix.txt')

       read(12,*)

    Else

       print *, 'NO COVARIANCE MATRIX FOUND IN OUTPUT FOLDER'

       stop

    End If

    Do index1=1,number_of_parameters

        read(12,*) matrix(index1,1:number_of_parameters)

    End Do

    close(12)

    call dsyev(JOBZ,UPLO,number_of_parameters,matrix,number_of_parameters,W,WORK,LWORK,INFO)

    If (INFO .eq. 0) then
 
        pos_def = .true.
        
        Do index1=1,number_of_parameters
         
            If (W(index1) .le. 0.d0) then

                pos_def = .false.

                exit

            End If

        End Do
      
        If (pos_def) then

            open(12,file='./output/chains/covariance_matrix.txt')

            read(12,*)

            Do index1=1,number_of_parameters

                read(12,*) matrix1(index1,1:number_of_parameters)

            End Do

            close(12)

        Else

            print *,'COVARIANCE MATRIX IS NOT POSITIVE DEFINITE, KEEPING CURRENT COVARIANCE MATRIX'
            
        End If

    Else

        print *,'EIGENVALUES WERE NOT COMPUTED'

    End If

end subroutine read_covariance_matrix_mcmc

subroutine read_bestfit_mcmc(vector)
    use fiducial
    Implicit none
    Real*8,dimension(number_of_parameters) :: vector
    Integer*4 :: index1
    open(12,file='./output/chains/bestfit.txt')
    Do index1=1,number_of_parameters
        read(12,*) vector(index1)
    End Do
    close(12)
end subroutine read_bestfit_mcmc

subroutine read_means_mcmc(vector)
    use fiducial
    Implicit none
    Real*8,dimension(number_of_parameters) :: vector
    Integer*4 :: index1

    open(12,file='./output/chains/means.txt')

    Do index1=1,number_of_parameters

        read(12,*) vector(index1)

    End Do

    close(12)

end subroutine read_means_mcmc

subroutine read_data(Cl,u,param_name,param_value,lensing_flag,fiducial_flag,El_Cl_flag)
    use fiducial
    Implicit none
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 m,u,p,i
    character(len=15) :: param_name
    character*16 :: param_value
    logical :: lensing_flag,fiducial_flag,El_Cl_flag

    If ((fiducial_flag .and. lensing_flag) .and. El_Cl_flag) then 
       open(u,file='./data/Cl_fiducial_lensing_cl.dat')
    else if ((fiducial_flag .and. lensing_flag) .and. .not.El_Cl_flag) then
       open(u,file='./data/El_cl.dat')
    else if ((fiducial_flag .and. .not.lensing_flag) .and. .not.El_Cl_flag) then
       open(u,file='./data/El_nl_cl.dat')
    else if ((fiducial_flag .and. .not.lensing_flag) .and. El_Cl_flag) then
        open(u,file='./data/Cl_fiducial_no_lensing_cl.dat')
    else if ((.not.fiducial_flag .and. lensing_flag) .and. El_Cl_flag) then
        open(u,file='./data/Cl_'//trim(param_name)//'_'//trim(param_value)//'_lensing_cl.dat')
    else if ((.not.fiducial_flag .and. .not.lensing_flag) .and. El_Cl_flag) then
        open(u,file='./data/Cl_'//trim(param_name)//'_'//trim(param_value)//'_no_lensing_cl.dat')
    End If
  
    Do m=-5,lmax
        If (m .le. 1) then
            read(u,*)
        else
            read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
            Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),&
            Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)

!            read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
!            Cl(m,1,6),Cl(m,1,7),Cl(m,1,8),Cl(m,1,9),Cl(m,1,10),Cl(m,2,2),Cl(m,2,3),&
!            Cl(m,2,4),Cl(m,2,5),Cl(m,2,6),Cl(m,2,7),Cl(m,2,8),Cl(m,2,9),Cl(m,2,10),&
!            Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),Cl(m,3,6),Cl(m,3,7),Cl(m,3,8),Cl(m,3,9),&
!            Cl(m,3,10),Cl(m,4,4),Cl(m,4,5),Cl(m,4,6),Cl(m,4,7),Cl(m,4,8),Cl(m,4,9),&
!            Cl(m,4,10),Cl(m,5,5),Cl(m,5,6),Cl(m,5,7),Cl(m,5,8),Cl(m,5,9),Cl(m,5,10),&
!            Cl(m,6,6),Cl(m,6,7),Cl(m,6,8),Cl(m,6,9),Cl(m,6,10),Cl(m,7,7),Cl(m,7,8),&
!            Cl(m,7,9),Cl(m,7,10),Cl(m,8,8),Cl(m,8,9),Cl(m,8,10),Cl(m,9,9),Cl(m,9,10),&
!            Cl(m,10,10)

            Do p=1,nbins
                Do i=1,nbins
                    If (p .gt. i) then
                        Cl(m,p,i) = Cl(m,i,p)
                    End If
                End Do
            End Do
        End If    
    End Do
    close(u)
end subroutine read_data

subroutine read_Cl(Cl,u,lensing_flag)
    use fiducial
    Implicit none
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 :: m,u,p,i
    logical :: lensing_flag
    character(len=*),parameter :: fmt = '(i2.2)'
    character*16 :: string

    If (testing_precision) then

       write(string,fmt) u
       
       open(u,file='./data/Cl_'//trim(string)//'_cl.dat')

    Else
       If (lensing_flag)  then 
          open(u,file='./output/current_euclid_galaxy_lensing_cl.dat')
       else 
          open(u,file='./output/current_euclid_galaxy_cl.dat')
       End If
    End If

    Do m=-5,lmax
        If (m .le. 1) then
            read(u,*)
        else
            read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
            Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),&
            Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)

!            read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
!            Cl(m,1,6),Cl(m,1,7),Cl(m,1,8),Cl(m,1,9),Cl(m,1,10),Cl(m,2,2),Cl(m,2,3),&
!            Cl(m,2,4),Cl(m,2,5),Cl(m,2,6),Cl(m,2,7),Cl(m,2,8),Cl(m,2,9),Cl(m,2,10),&
!            Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),Cl(m,3,6),Cl(m,3,7),Cl(m,3,8),Cl(m,3,9),&
!            Cl(m,3,10),Cl(m,4,4),Cl(m,4,5),Cl(m,4,6),Cl(m,4,7),Cl(m,4,8),Cl(m,4,9),&
!            Cl(m,4,10),Cl(m,5,5),Cl(m,5,6),Cl(m,5,7),Cl(m,5,8),Cl(m,5,9),Cl(m,5,10),&
!            Cl(m,6,6),Cl(m,6,7),Cl(m,6,8),Cl(m,6,9),Cl(m,6,10),Cl(m,7,7),Cl(m,7,8),&
!            Cl(m,7,9),Cl(m,7,10),Cl(m,8,8),Cl(m,8,9),Cl(m,8,10),Cl(m,9,9),Cl(m,9,10),&
!            Cl(m,10,10)

            Do p=1,nbins
                Do i=1,nbins
                    If (p .gt. i) then
                        Cl(m,p,i) = Cl(m,i,p)
                    End If
                End Do
            End Do
        End If    
    End Do
    close(u)
end subroutine read_Cl

subroutine read_Cl_mcmc(Cl,u,lensing_flag,job)
    use fiducial
    Implicit none
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 :: m,u,p,i
    logical :: lensing_flag
    character(len=*),parameter :: fmt = '(i2.2)'
    Character(len=10) :: job

    If (lensing_flag)  then 
    
       open(u,file= './output/current_euclid_galaxy_lensing_cl_'//trim(job)//'_cl.dat')

    Else 

       open(u,file= PATH_TO_CURRENT_CL//trim(job)//'_cl.dat')

    End If

    Do m=-5,lmax

       If (m .le. 1) then

          read(u,*)

       Else

          read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
               Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),&
               Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)
          
          Do p=1,nbins

             Do i=1,nbins

                If (p .gt. i) then

                   Cl(m,p,i) = Cl(m,i,p)

                End If

             End Do

          End Do

       End If

    End Do

    close(u)

end subroutine read_Cl_mcmc

subroutine read_derivative(Cl,u,param_name,lensing_flag)
    use fiducial
    Implicit none
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 m,u,p,i
    character(len=15) :: param_name
    logical :: lensing_flag
    If (lensing_flag) then 
        open(u,file='./output/dCl_'//trim(param_name)//'_lensing.dat')
    else 
        open(u,file='./output/dCl_'//trim(param_name)//'_.dat')
    End If
  
    Do m=-5,lmax
        If (m .le. 1) then
            read(u,*)
        else
            read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
            Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),&
            Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)
 
!            read(u,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
!            Cl(m,1,6),Cl(m,1,7),Cl(m,1,8),Cl(m,1,9),Cl(m,1,10),Cl(m,2,2),Cl(m,2,3),&
!            Cl(m,2,4),Cl(m,2,5),Cl(m,2,6),Cl(m,2,7),Cl(m,2,8),Cl(m,2,9),Cl(m,2,10),&
!            Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),Cl(m,3,6),Cl(m,3,7),Cl(m,3,8),Cl(m,3,9),&
!            Cl(m,3,10),Cl(m,4,4),Cl(m,4,5),Cl(m,4,6),Cl(m,4,7),Cl(m,4,8),Cl(m,4,9),&
!            Cl(m,4,10),Cl(m,5,5),Cl(m,5,6),Cl(m,5,7),Cl(m,5,8),Cl(m,5,9),Cl(m,5,10),&
!            Cl(m,6,6),Cl(m,6,7),Cl(m,6,8),Cl(m,6,9),Cl(m,6,10),Cl(m,7,7),Cl(m,7,8),&
!            Cl(m,7,9),Cl(m,7,10),Cl(m,8,8),Cl(m,8,9),Cl(m,8,10),Cl(m,9,9),Cl(m,9,10),&
!            Cl(m,10,10)

            Do p=1,nbins
                Do i=1,nbins
                    If (p .gt. i) then
                        Cl(m,p,i) = Cl(m,i,p)
                    End If
                End Do
            End Do
        End If    
    End Do
    close(u)
end subroutine read_derivative

subroutine write_Cl_syst(Cl,u)
    use fiducial
    Implicit none
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 m,u
    open(u,file='./output/Cl_syst.dat')
    Do m=-5,lmax
        If (m .le. 1) then
            write(u,*) '#'
        else
            write(u,*) m, Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
            Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),&
            Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)

!            write(u,*) m, Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),&
!            Cl(m,1,5),Cl(m,1,6),Cl(m,1,7),Cl(m,1,8),Cl(m,1,9),Cl(m,1,10),Cl(m,2,2),&
!            Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,2,6),Cl(m,2,7),Cl(m,2,8),Cl(m,2,9),&
!            Cl(m,2,10),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),Cl(m,3,6),Cl(m,3,7),Cl(m,3,8),&
!            Cl(m,3,9),Cl(m,3,10),Cl(m,4,4),Cl(m,4,5),Cl(m,4,6),Cl(m,4,7),Cl(m,4,8),&
!            Cl(m,4,9),Cl(m,4,10),Cl(m,5,5),Cl(m,5,6),Cl(m,5,7),Cl(m,5,8),Cl(m,5,9),&
!            Cl(m,5,10),Cl(m,6,6),Cl(m,6,7),Cl(m,6,8),Cl(m,6,9),Cl(m,6,10),Cl(m,7,7),&
!            Cl(m,7,8),Cl(m,7,9),Cl(m,7,10),Cl(m,8,8),Cl(m,8,9),Cl(m,8,10),Cl(m,9,9),&
!            Cl(m,9,10),Cl(m,10,10)

        End If    
    End Do
    close(u)
end subroutine write_Cl_syst


subroutine write_data(Cl,u,param_name,lensing_flag)
    use fiducial
    Implicit none
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 m,u
    character(len=15) :: param_name
    logical :: lensing_flag
    If (lensing_flag) then 
        open(u,file='./output/dCl_'//trim(param_name)//'_lensing.dat')
    else
        open(u,file='./output/dCl_'//trim(param_name)//'_.dat')
    End If

    Do m=-5,lmax
        If (m .le. 1) then
            write(u,*) '#'
        else
            write(u,*) m, Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
            Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),&
            Cl(m,3,5),Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)
 
!            write(u,*) m, Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),&
!            Cl(m,1,5),Cl(m,1,6),Cl(m,1,7),Cl(m,1,8),Cl(m,1,9),Cl(m,1,10),Cl(m,2,2),&
!            Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,2,6),Cl(m,2,7),Cl(m,2,8),Cl(m,2,9),&
!            Cl(m,2,10),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),Cl(m,3,6),Cl(m,3,7),Cl(m,3,8),&
!            Cl(m,3,9),Cl(m,3,10),Cl(m,4,4),Cl(m,4,5),Cl(m,4,6),Cl(m,4,7),Cl(m,4,8),&
!            Cl(m,4,9),Cl(m,4,10),Cl(m,5,5),Cl(m,5,6),Cl(m,5,7),Cl(m,5,8),Cl(m,5,9),&
!            Cl(m,5,10),Cl(m,6,6),Cl(m,6,7),Cl(m,6,8),Cl(m,6,9),Cl(m,6,10),Cl(m,7,7),&
!            Cl(m,7,8),Cl(m,7,9),Cl(m,7,10),Cl(m,8,8),Cl(m,8,9),Cl(m,8,10),Cl(m,9,9),&
!            Cl(m,9,10),Cl(m,10,10)

        End If    
    End Do
    close(u)
end subroutine write_data

subroutine compute_b_lambda_alpha()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: i,j

    Do i=1,number_of_parameters
        Do j=1,number_of_parameters
            b_lambda(i) = b_lambda(i) + inv_F_ab(i,j)*B_beta(j) 
        End Do
    End Do

    open(11,file='./output/b_lambda_alpha.dat')

    write(11,*) '# Order of parameters in columns is as follows: '
    write(11,*) '# omega_b, omega_cdm, n_s, ln(10^10*A_s), H_0, m_ncdm, MG_beta2 '
    write(11,'(7es25.10)') b_lambda(1), b_lambda(2), b_lambda(3),&
                           b_lambda(4), b_lambda(5), b_lambda(6), b_lambda(7)

    close(11)

end subroutine compute_b_lambda_alpha

function galaxy_distribution(z)
    Implicit none
    Real*8 :: zmean,z0,z,galaxy_distribution
    zmean = 0.9d0
    z0 = zmean/1.412d0
    galaxy_distribution = z**2*exp(-(z/z0)**(1.5))
end function galaxy_distribution


subroutine bin_centers_widths_bias(z_bin_centers,z_bin_widths,z_bin_bias,s_z_mag_bias)
    use fiducial
    Implicit none
    
    Real*8 :: n_tot, gd_1, gd_2, gal_count, z
    Integer*4 :: m,n,i
    Real*8,dimension(int((zmax - zmin)/dz)) :: z_array
    Real*8,dimension(int(nbins+1)) :: z_bin_edges
    Real*8,dimension(nbins) :: z_bin_centers, z_bin_widths, z_bin_bias,s_z_mag_bias
    Real*8,parameter :: s_0 = 0.1194d0
    Real*8,parameter :: s_1 = 0.2122d0
    Real*8,parameter :: s_2 = -0.0671d0
    Real*8,parameter :: s_3 = 0.1031d0
    Integer*4 :: p,nb
    p = int((zmax - zmin)/dz)
    nb = nbins + 1
    
    z_array(1) = zmin
    Do m=2,p
        z_array(m) = z_array(m-1) + dz
    End do
    z_array(p) = zmax

    n_tot = 0.d0
    Do m=1,p-1
        gd_1 = galaxy_distribution(z_array(m))
        gd_2 = galaxy_distribution(z_array(m+1))
        n_tot = (gd_1 + gd_2)/2.*dz + n_tot 
    End Do
    
    z_bin_edges(1) = zmin
    Do n=1,nbins
        gal_count = 0.d0
        z = z_bin_edges(n)
        Do i=1,10000
            If (gal_count .gt. n_tot/nbins) exit
            gd_1 = galaxy_distribution(z)
            gd_2 = galaxy_distribution(z+dz)
            gal_count = (gd_1 + gd_2)/2.*dz + gal_count
            z = z + dz 
        End Do
        z_bin_edges(n+1) = z
    End Do
    z_bin_edges(nbins+1) = zmax

    Do n=2,nbins+1
        z_bin_centers(n-1) = (z_bin_edges(n) + z_bin_edges(n-1))/2.
        z_bin_widths(n-1) = (z_bin_edges(n) - z_bin_edges(n-1))/2.
        z_bin_bias(n-1) = sqrt(1.d0 + z_bin_centers(n-1))
        s_z_mag_bias(n-1) = s_0 + s_1*z_bin_centers(n-1) + s_2*z_bin_centers(n-1)**2 + s_3*z_bin_centers(n-1)**3
    End Do

end subroutine bin_centers_widths_bias

function indices_I_P_from_i_j(m,n)
    use fiducial
    Implicit none
    Integer*4,dimension(nbins,nbins) :: I
    Integer*4 :: m,n,p,q,counter,indices_I_P_from_i_j
    counter = 1
    If (m .gt. n) then 
        print *,'index n must be greater than m in function "index_I_P"'
        stop
    Else If ((m .gt. nbins) .or. (n .gt. nbins)) then
        print *, 'Arguments of function "indices_I_P" must not be greater than number of bins'
        stop
    End If
    Do p=1,nbins
        Do q=1,nbins
            If (p .le. q) then
                I(p,q) = counter
                counter = counter + 1 
            End If
        End Do
    End Do
    indices_I_P_from_i_j = I(m,n)
end function indices_I_P_from_i_j

function indices_I_P_from_i_j_oa(m,n)
    use fiducial
    Implicit none
    Integer*4,dimension(nbins,nbins) :: I
    Integer*4 :: m,n,p,q,counter,indices_I_P_from_i_j_oa
    counter = 1
    If (m .ne. n) then 
        print *,'index n must be equal to m in function "index_I_P"'
        stop
    Else If ((m .gt. nbins) .or. (n .gt. nbins)) then
        print *, 'Arguments of function "indices_I_P" must not be greater than number of bins'
        stop
    End If
    Do p=1,nbins
        Do q=1,nbins
            If (p .eq. q) then
                I(p,q) = counter
                counter = counter + 1 
            End If
        End Do
    End Do
    indices_I_P_from_i_j_oa = I(m,n)
end function indices_I_P_from_i_j_oa

subroutine inverting_matrix()
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: l,M,N,LDA,INFO,LWORK,i,j
    Real*8,dimension(max(1,nbins*(nbins+1)/2),nbins*(nbins+1)/2) :: A
    Integer*4,dimension(min(nbins*(nbins+1)/2,nbins*(nbins+1)/2)) :: IPIV
    Real*8,dimension(max(1,max(1,nbins*(nbins+1)/2))) :: WORK
    M = nbins*(nbins+1)/2
    N = M
    LDA = max(1,M)
    LWORK = max(1,N)

    Do l=lmin,lmax
        Do i=1,M
            Do j=1,M
                A(i,j) = cov_l_IP(l,i,j)
            End Do
        End Do

        call dgetrf(M,N,A,LDA,IPIV,INFO)

        call dgetri(N,A,LDA,IPIV,WORK,LWORK,INFO)

        Do i=1,M
            Do j=1,M
                inv_cov_l_IP(l,i,j) = A(i,j)
            End Do
        End Do
    End Do
    
end subroutine inverting_matrix

subroutine inverting_matrix_oa()
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: l,M,N,LDA,INFO,LWORK,i,j
    Real*8,dimension(max(1,nbins),nbins) :: A
    Integer*4,dimension(min(nbins,nbins)) :: IPIV
    Real*8,dimension(max(1,max(1,nbins))) :: WORK
    M = nbins
    N = M
    LDA = max(1,M)
    LWORK = max(1,N)

    Do l=lmin,lmax
        Do i=1,M
            Do j=1,M
                A(i,j) = cov_l_IP_oa(l,i,j)
            End Do
        End Do

        call dgetrf(M,N,A,LDA,IPIV,INFO)

        call dgetri(N,A,LDA,IPIV,WORK,LWORK,INFO)

        Do i=1,M
            Do j=1,M
                inv_cov_l_IP_oa(l,i,j) = A(i,j)
            End Do
        End Do
    End Do
    
end subroutine inverting_matrix_oa

subroutine compute_inverse_fisher_matrix()
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: M,N,LDA,INFO,LWORK,i,j
    Real*8,dimension(max(1,7),7) :: A
    Integer*4,dimension(min(7,7)) :: IPIV
    Real*8,dimension(max(1,max(1,7))) :: WORK
    M = 7
    N = M
    LDA = max(1,M)
    LWORK = max(1,N)

    Do i=1,M
        Do j=1,M
            A(i,j) = F_ab(i,j)
        End Do
    End Do

    call dgetrf(M,N,A,LDA,IPIV,INFO)

    call dgetri(N,A,LDA,IPIV,WORK,LWORK,INFO)

    Do i=1,M
        Do j=1,M
            inv_F_ab(i,j) = A(i,j)
        End Do
    End Do
    
    open(11,file='./output/inverse_fisher_matrix.dat')
    write(11,*) '# Order of parameters in rows and columns is as follows: '
    write(11,*) '# omega_b, omega_cdm, n_s, ln(10^10*A_s), H_0, m_ncdm, MG_beta2 '
    write(11,'(7es25.10)') inv_F_ab(1,1), inv_F_ab(1,2), inv_F_ab(1,3), inv_F_ab(1,4), inv_F_ab(1,5), inv_F_ab(1,6), inv_F_ab(1,7)
    write(11,'(7es25.10)') inv_F_ab(2,1), inv_F_ab(2,2), inv_F_ab(2,3), inv_F_ab(2,4), inv_F_ab(2,5), inv_F_ab(2,6), inv_F_ab(2,7)
    write(11,'(7es25.10)') inv_F_ab(3,1), inv_F_ab(3,2), inv_F_ab(3,3), inv_F_ab(3,4), inv_F_ab(3,5), inv_F_ab(3,6), inv_F_ab(3,7)
    write(11,'(7es25.10)') inv_F_ab(4,1), inv_F_ab(4,2), inv_F_ab(4,3), inv_F_ab(4,4), inv_F_ab(4,5), inv_F_ab(4,6), inv_F_ab(4,7)
    write(11,'(7es25.10)') inv_F_ab(5,1), inv_F_ab(5,2), inv_F_ab(5,3), inv_F_ab(5,4), inv_F_ab(5,5), inv_F_ab(5,6), inv_F_ab(5,7)
    write(11,'(7es25.10)') inv_F_ab(6,1), inv_F_ab(6,2), inv_F_ab(6,3), inv_F_ab(6,4), inv_F_ab(6,5), inv_F_ab(6,6), inv_F_ab(6,7)
    write(11,'(7es25.10)') inv_F_ab(7,1), inv_F_ab(7,2), inv_F_ab(7,3), inv_F_ab(7,4), inv_F_ab(7,5), inv_F_ab(7,6), inv_F_ab(7,7)
    close(11)

end subroutine compute_inverse_fisher_matrix

subroutine read_inverse_fisher_matrix(matrix1)
    use fiducial
    Implicit none
    Real*8,dimension(number_of_parameters,number_of_parameters) :: matrix,matrix1
    Integer*4 :: index1,INFO
    Integer*4,parameter :: LWORK = max(1,3*number_of_parameters-1)
    Real*8,dimension(max(1,LWORK)) :: WORK
    Real*8,dimension(number_of_parameters) :: W
    Character*1,parameter :: JOBZ = 'N'
    Character*1,parameter :: UPLO = 'U'
    Logical :: pos_def,exist 
 
    inquire(file='./output/inverse_fisher_matrix.dat',exist=exist)

    If (exist) then

       open(12,file='./output/inverse_fisher_matrix.dat')

    Else

       print *, 'NO INVERSE OF FISHER MATRIX FOUND IN OUTPUT FOLDER'

       stop

    End If
    
    read(12,*)

    read(12,*)

    Do index1=1,number_of_parameters

        read(12,'(7es25.10)') matrix(index1,1:number_of_parameters)

    End Do

    close(12)

    call dsyev(JOBZ,UPLO,number_of_parameters,matrix,number_of_parameters,W,WORK,LWORK,INFO)

    If (INFO .eq. 0) then
 
        pos_def = .true.
        
        Do index1=1,number_of_parameters
         
            If (W(index1) .le. 0.d0) then

                pos_def = .false.

                exit

            End If

        End Do
      
        If (pos_def) then

            open(12,file='./output/inverse_fisher_matrix.dat')

            read(12,*)

            read(12,*)

            Do index1=1,number_of_parameters

                read(12,'(7es25.10)') matrix1(index1,1:number_of_parameters)

            End Do

            close(12)

        Else

            print *,'INVERSE OF FISHER MATRIX IS NOT POSITIVE DEFINITE '
            
        End If

    Else

        print *,'EIGENVALUES WERE NOT COMPUTED FOR INVERSE OF FISHER MATRIX'

    End If

end subroutine read_inverse_fisher_matrix

subroutine write_inverse_covariance_matrix(l)
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: M,l,i,j
    character(len=8) :: fmt
    character(len=4) :: ls
    M = nbins*(nbins+1)/2
    fmt = '(i4.4)'
    write(ls,fmt) l
    If ((l .ge. lmin) .and. (l .le. lmax)) then 
        open(15,file='./output/inverse_covariance_matrix_l_'//trim(ls)//'.dat')
    else
        print *, 'l is out of bounds '
        stop
    End If

    write(15,*) '# Inverse of the covariance matrix for super indices I and J'
    Do i=1,M
        write(15,'(55es18.10)') (inv_cov_l_IP(l,i,j),j=1,M) 
    End Do
    close(15)
end subroutine write_inverse_covariance_matrix

subroutine write_covariance_matrix(l)
    use fiducial
    use arrays
    Implicit none
    Integer*4 :: M,l,i,j
    character(len=8) :: fmt
    character(len=4) :: ls
    M = nbins*(nbins+1)/2
    fmt = '(i4.4)'
    write(ls,fmt) l
    If ((l .ge. lmin) .and. (l .le. lmax)) then 
        open(15,file='./output/covariance_matrix_l_'//trim(ls)//'.dat')
    else
        print *, 'l is out of bounds '
        stop
    End If

    write(15,*) '# Covariance matrix for super indices I and J'
    Do i=1,M
        write(15,'(55es18.10)') (cov_l_IP(l,i,j),j=1,M) 
    End Do
    close(15)
end subroutine write_covariance_matrix


subroutine compute_fisher_matrix(lensing_flag,autocorr_flag)
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: l,i,j,p,q
    logical :: lensing_flag,autocorr_flag
    character(len=15),dimension(7) :: params
    params(1) = 'omega_b'
    params(2) = 'omega_cdm'
    params(3) = 'n_s'
    params(4) = 'A_s'
    params(5) = 'H0'
    params(6) = 'm_ncdm'
    params(7) = 'MG_beta2'

    F_ab(:,:) = 0.d0
    F_ab_nl(:,:) = 0.d0
    If (lensing_flag) then 
        call read_derivative(d1,10,params(1),.true.)
        call read_derivative(d2,11,params(2),.true.)
        call read_derivative(d3,12,params(3),.true.)
        call read_derivative(d4,13,params(4),.true.)
        call read_derivative(d5,14,params(5),.true.)
        call read_derivative(d6,15,params(6),.true.)
        call read_derivative(d7,16,params(7),.true.)

        Do l=lmin,lmax
            Do i=1,nbins
                Do j=1,nbins
                    Do p=1,nbins
                        Do q=1,nbins
                            If (autocorr_flag .and. ((i .eq. j) .and. (p .eq. q))) then
                                F_ab(1,1) = F_ab(1,1) + d1(l,i,j)*d1(l,p,q)*inv_cov_oa(l,i,j,p,q) 
                                F_ab(1,2) = F_ab(1,2) + d1(l,i,j)*d2(l,p,q)*inv_cov_oa(l,i,j,p,q) 
                                F_ab(1,3) = F_ab(1,3) + d1(l,i,j)*d3(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(1,4) = F_ab(1,4) + d1(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(1,5) = F_ab(1,5) + d1(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(1,6) = F_ab(1,6) + d1(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(1,7) = F_ab(1,7) + d1(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(2,2) = F_ab(2,2) + d2(l,i,j)*d2(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(2,3) = F_ab(2,3) + d2(l,i,j)*d3(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(2,4) = F_ab(2,4) + d2(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(2,5) = F_ab(2,5) + d2(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(2,6) = F_ab(2,6) + d2(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(2,7) = F_ab(2,7) + d2(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(3,3) = F_ab(3,3) + d3(l,i,j)*d3(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(3,4) = F_ab(3,4) + d3(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(3,5) = F_ab(3,5) + d3(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(3,6) = F_ab(3,6) + d3(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(3,7) = F_ab(3,7) + d3(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(4,4) = F_ab(4,4) + d4(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(4,5) = F_ab(4,5) + d4(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(4,6) = F_ab(4,6) + d4(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(4,7) = F_ab(4,7) + d4(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(5,5) = F_ab(5,5) + d5(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(5,6) = F_ab(5,6) + d5(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(5,7) = F_ab(5,7) + d5(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(6,6) = F_ab(6,6) + d6(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(6,7) = F_ab(6,7) + d6(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab(7,7) = F_ab(7,7) + d7(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                            Else If (.not.autocorr_flag .and. ((i .le. j) .and. (p .le. q))) then
                                F_ab(1,1) = F_ab(1,1) + d1(l,i,j)*d1(l,p,q)*inv_cov(l,i,j,p,q) 
                                F_ab(1,2) = F_ab(1,2) + d1(l,i,j)*d2(l,p,q)*inv_cov(l,i,j,p,q) 
                                F_ab(1,3) = F_ab(1,3) + d1(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(1,4) = F_ab(1,4) + d1(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(1,5) = F_ab(1,5) + d1(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(1,6) = F_ab(1,6) + d1(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(1,7) = F_ab(1,7) + d1(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(2,2) = F_ab(2,2) + d2(l,i,j)*d2(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(2,3) = F_ab(2,3) + d2(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(2,4) = F_ab(2,4) + d2(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(2,5) = F_ab(2,5) + d2(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(2,6) = F_ab(2,6) + d2(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(2,7) = F_ab(2,7) + d2(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(3,3) = F_ab(3,3) + d3(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(3,4) = F_ab(3,4) + d3(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(3,5) = F_ab(3,5) + d3(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(3,6) = F_ab(3,6) + d3(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(3,7) = F_ab(3,7) + d3(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(4,4) = F_ab(4,4) + d4(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(4,5) = F_ab(4,5) + d4(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(4,6) = F_ab(4,6) + d4(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(4,7) = F_ab(4,7) + d4(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(5,5) = F_ab(5,5) + d5(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(5,6) = F_ab(5,6) + d5(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(5,7) = F_ab(5,7) + d5(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(6,6) = F_ab(6,6) + d6(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(6,7) = F_ab(6,7) + d6(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab(7,7) = F_ab(7,7) + d7(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                            End If
                        End Do
                    End Do
                End Do
            End Do
        End Do

        F_ab(1,4) = F_ab(1,4)*A_s
        F_ab(2,4) = F_ab(2,4)*A_s
        F_ab(3,4) = F_ab(3,4)*A_s
        F_ab(4,4) = F_ab(4,4)*A_s**2
        F_ab(4,5) = F_ab(4,5)*A_s
        F_ab(4,6) = F_ab(4,6)*A_s
        F_ab(4,7) = F_ab(4,7)*A_s

        Do i=1,7
            Do j=1,7
                If (i .gt. j) then
                    F_ab(i,j) = F_ab(j,i)
                End If
            End Do
        End Do
        
        If (autocorr_flag) then
            open(11,file='./output/fisher_matrix_lensing_only_autocorrelations.dat')
        Else
            open(11,file='./output/fisher_matrix_lensing.dat')
        End If
        write(11,*) '# Order of parameters in rows and columns is as follows: '
        write(11,*) '# omega_b, omega_cdm, n_s, ln(10^10*A_s), H_0, m_ncdm, MG_beta2 '
        write(11,'(7es25.10)') F_ab(1,1), F_ab(1,2), F_ab(1,3), F_ab(1,4), F_ab(1,5), F_ab(1,6), F_ab(1,7)
        write(11,'(7es25.10)') F_ab(2,1), F_ab(2,2), F_ab(2,3), F_ab(2,4), F_ab(2,5), F_ab(2,6), F_ab(2,7)
        write(11,'(7es25.10)') F_ab(3,1), F_ab(3,2), F_ab(3,3), F_ab(3,4), F_ab(3,5), F_ab(3,6), F_ab(3,7)
        write(11,'(7es25.10)') F_ab(4,1), F_ab(4,2), F_ab(4,3), F_ab(4,4), F_ab(4,5), F_ab(4,6), F_ab(4,7)
        write(11,'(7es25.10)') F_ab(5,1), F_ab(5,2), F_ab(5,3), F_ab(5,4), F_ab(5,5), F_ab(5,6), F_ab(5,7)
        write(11,'(7es25.10)') F_ab(6,1), F_ab(6,2), F_ab(6,3), F_ab(6,4), F_ab(6,5), F_ab(6,6), F_ab(6,7)
        write(11,'(7es25.10)') F_ab(7,1), F_ab(7,2), F_ab(7,3), F_ab(7,4), F_ab(7,5), F_ab(7,6), F_ab(7,7)
        close(11)
    else 
        call read_derivative(d1,16,params(1),.false.)
        call read_derivative(d2,17,params(2),.false.)
        call read_derivative(d3,18,params(3),.false.)
        call read_derivative(d4,19,params(4),.false.)
        call read_derivative(d5,20,params(5),.false.)
        call read_derivative(d6,21,params(6),.false.)
        call read_derivative(d7,22,params(7),.false.)
        Do l=lmin,lmax
            Do i=1,nbins
                Do j=1,nbins
                    Do p=1,nbins
                        Do q=1,nbins
                            If (autocorr_flag .and. ((i .eq. j) .and. (p .eq. q))) then
                                F_ab_nl(1,1) = F_ab_nl(1,1) + d1(l,i,j)*d1(l,p,q)*inv_cov_oa(l,i,j,p,q) 
                                F_ab_nl(1,2) = F_ab_nl(1,2) + d1(l,i,j)*d2(l,p,q)*inv_cov_oa(l,i,j,p,q) 
                                F_ab_nl(1,3) = F_ab_nl(1,3) + d1(l,i,j)*d3(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(1,4) = F_ab_nl(1,4) + d1(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(1,5) = F_ab_nl(1,5) + d1(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(1,6) = F_ab_nl(1,6) + d1(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(1,7) = F_ab_nl(1,7) + d1(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(2,2) = F_ab_nl(2,2) + d2(l,i,j)*d2(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(2,3) = F_ab_nl(2,3) + d2(l,i,j)*d3(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(2,4) = F_ab_nl(2,4) + d2(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(2,5) = F_ab_nl(2,5) + d2(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(2,6) = F_ab_nl(2,6) + d2(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(2,7) = F_ab_nl(2,7) + d2(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(3,3) = F_ab_nl(3,3) + d3(l,i,j)*d3(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(3,4) = F_ab_nl(3,4) + d3(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(3,5) = F_ab_nl(3,5) + d3(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(3,6) = F_ab_nl(3,6) + d3(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(3,7) = F_ab_nl(3,7) + d3(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(4,4) = F_ab_nl(4,4) + d4(l,i,j)*d4(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(4,5) = F_ab_nl(4,5) + d4(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(4,6) = F_ab_nl(4,6) + d4(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(4,7) = F_ab_nl(4,7) + d4(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(5,5) = F_ab_nl(5,5) + d5(l,i,j)*d5(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(5,6) = F_ab_nl(5,6) + d5(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(5,7) = F_ab_nl(5,7) + d5(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(6,6) = F_ab_nl(6,6) + d6(l,i,j)*d6(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(6,7) = F_ab_nl(6,7) + d6(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                                F_ab_nl(7,7) = F_ab_nl(7,7) + d7(l,i,j)*d7(l,p,q)*inv_cov_oa(l,i,j,p,q)
                            Else If (.not.autocorr_flag .and. ((i .le. j) .and. (p .le. q))) then
                                F_ab_nl(1,1) = F_ab_nl(1,1) + d1(l,i,j)*d1(l,p,q)*inv_cov(l,i,j,p,q) 
                                F_ab_nl(1,2) = F_ab_nl(1,2) + d1(l,i,j)*d2(l,p,q)*inv_cov(l,i,j,p,q) 
                                F_ab_nl(1,3) = F_ab_nl(1,3) + d1(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(1,4) = F_ab_nl(1,4) + d1(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(1,5) = F_ab_nl(1,5) + d1(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(1,6) = F_ab_nl(1,6) + d1(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(1,7) = F_ab_nl(1,7) + d1(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(2,2) = F_ab_nl(2,2) + d2(l,i,j)*d2(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(2,3) = F_ab_nl(2,3) + d2(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(2,4) = F_ab_nl(2,4) + d2(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(2,5) = F_ab_nl(2,5) + d2(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(2,6) = F_ab_nl(2,6) + d2(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(2,7) = F_ab_nl(2,7) + d2(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(3,3) = F_ab_nl(3,3) + d3(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(3,4) = F_ab_nl(3,4) + d3(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(3,5) = F_ab_nl(3,5) + d3(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(3,6) = F_ab_nl(3,6) + d3(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(3,7) = F_ab_nl(3,7) + d3(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(4,4) = F_ab_nl(4,4) + d4(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(4,5) = F_ab_nl(4,5) + d4(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(4,6) = F_ab_nl(4,6) + d4(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(4,7) = F_ab_nl(4,7) + d4(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(5,5) = F_ab_nl(5,5) + d5(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(5,6) = F_ab_nl(5,6) + d5(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(5,7) = F_ab_nl(5,7) + d5(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(6,6) = F_ab_nl(6,6) + d6(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(6,7) = F_ab_nl(6,7) + d6(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                                F_ab_nl(7,7) = F_ab_nl(7,7) + d7(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                            End If
                        End Do
                    End Do
                End Do
            End Do
        End Do

        F_ab_nl(1,4) = F_ab_nl(1,4)*A_s
        F_ab_nl(2,4) = F_ab_nl(2,4)*A_s
        F_ab_nl(3,4) = F_ab_nl(3,4)*A_s
        F_ab_nl(4,4) = F_ab_nl(4,4)*A_s**2
        F_ab_nl(4,5) = F_ab_nl(4,5)*A_s
        F_ab_nl(4,6) = F_ab_nl(4,6)*A_s
        F_ab_nl(4,7) = F_ab_nl(4,7)*A_s

        Do i=1,7
            Do j=1,7
                If (i .gt. j) then
                    F_ab_nl(i,j) = F_ab_nl(j,i)
                End If
            End Do
        End Do
        If (autocorr_flag) then
            open(12,file='./output/fisher_matrix_only_autocorrelations.dat')
        Else
            open(12,file='./output/fisher_matrix.dat')
        End If
        write(12,*) '# Order of parameters in rows and columns is as follows: '
        write(12,*) '# omega_b, omega_cdm, n_s, ln(10^10*A_s), H_0, m_ncdm, MG_beta2 '
        write(12,'(7es25.10)') F_ab_nl(1,1), F_ab_nl(1,2), F_ab_nl(1,3), F_ab_nl(1,4), F_ab_nl(1,5), F_ab_nl(1,6), F_ab_nl(1,7)
        write(12,'(7es25.10)') F_ab_nl(2,1), F_ab_nl(2,2), F_ab_nl(2,3), F_ab_nl(2,4), F_ab_nl(2,5), F_ab_nl(2,6), F_ab_nl(2,7)
        write(12,'(7es25.10)') F_ab_nl(3,1), F_ab_nl(3,2), F_ab_nl(3,3), F_ab_nl(3,4), F_ab_nl(3,5), F_ab_nl(3,6), F_ab_nl(3,7)
        write(12,'(7es25.10)') F_ab_nl(4,1), F_ab_nl(4,2), F_ab_nl(4,3), F_ab_nl(4,4), F_ab_nl(4,5), F_ab_nl(4,6), F_ab_nl(4,7)
        write(12,'(7es25.10)') F_ab_nl(5,1), F_ab_nl(5,2), F_ab_nl(5,3), F_ab_nl(5,4), F_ab_nl(5,5), F_ab_nl(5,6), F_ab_nl(5,7)
        write(12,'(7es25.10)') F_ab_nl(6,1), F_ab_nl(6,2), F_ab_nl(6,3), F_ab_nl(6,4), F_ab_nl(6,5), F_ab_nl(6,6), F_ab_nl(6,7)
        write(12,'(7es25.10)') F_ab_nl(7,1), F_ab_nl(7,2), F_ab_nl(7,3), F_ab_nl(7,4), F_ab_nl(7,5), F_ab_nl(7,6), F_ab_nl(7,7)
        close(12)
    End If 
end subroutine compute_fisher_matrix

subroutine compute_B_beta()
    use arrays
    use fiducial
    Implicit none
    Integer*4 :: l,i,j,p,q
    character(len=15),dimension(7) :: params
    params(1) = 'omega_b'
    params(2) = 'omega_cdm'
    params(3) = 'n_s'
    params(4) = 'A_s'
    params(5) = 'H0'
    params(6) = 'm_ncdm'
    params(7) = 'MG_beta2'

    B_beta(:) = 0.d0

    call read_derivative(d1,10,params(1),.true.)
    call read_derivative(d2,11,params(2),.true.)
    call read_derivative(d3,12,params(3),.true.)
    call read_derivative(d4,13,params(4),.true.)
    call read_derivative(d5,14,params(5),.true.)
    call read_derivative(d6,15,params(6),.true.)
    call read_derivative(d7,16,params(7),.true.)

    Do l=lmin,lmax
        Do i=1,nbins
            Do j=1,nbins
                Do p=1,nbins
                    Do q=1,nbins
                        If ((i .le. j) .and. (p .le. q)) then
                            B_beta(1) = B_beta(1) + Cl_syst(l,i,j)*d1(l,p,q)*inv_cov(l,i,j,p,q) 
                            B_beta(2) = B_beta(2) + Cl_syst(l,i,j)*d2(l,p,q)*inv_cov(l,i,j,p,q) 
                            B_beta(3) = B_beta(3) + Cl_syst(l,i,j)*d3(l,p,q)*inv_cov(l,i,j,p,q)
                            B_beta(4) = B_beta(4) + Cl_syst(l,i,j)*d4(l,p,q)*inv_cov(l,i,j,p,q)
                            B_beta(5) = B_beta(5) + Cl_syst(l,i,j)*d5(l,p,q)*inv_cov(l,i,j,p,q)
                            B_beta(6) = B_beta(6) + Cl_syst(l,i,j)*d6(l,p,q)*inv_cov(l,i,j,p,q)
                            B_beta(7) = B_beta(7) + Cl_syst(l,i,j)*d7(l,p,q)*inv_cov(l,i,j,p,q)
                        End If
                    End Do
                End Do
            End Do
        End Do
    End Do

    B_beta(4) = B_beta(4)*A_s

    open(11,file='./output/B_beta.dat')

    write(11,*) '# Order of parameters in columns is as follows: '
    write(11,*) '# omega_b, omega_cdm, n_s, ln(10^10*A_s), H_0, m_ncdm, MG_beta2 '
    write(11,'(7es25.10)') B_beta(1), B_beta(2), B_beta(3), B_beta(4), B_beta(5), B_beta(6), B_beta(7)

    close(11)
    
end subroutine compute_B_beta


subroutine change_filename_format()
    use fiducial
    use arrays
    Implicit none
    character*16 :: string_omega_b, string_omega_cdm, string_n_s, string_A_s, string_H0, string_m_ncdm, string_MG_beta2
    character*16 :: omBfid,omCDMfid,nsfid,Asfid,H0fid,mncdmfid,MG_beta2fid
    character*16 :: somb,somc,sns,sAs,sH0,sm,sMG_beta2,fmt
    logical :: exist,exist2
    Integer*4 m,p
    character(len=15),dimension(14) :: params
    Real*8,dimension(0:4) :: AsF
    AsF(:) = 0.
    fmt = '(es16.10)'
    params(1) = 'omega_b'
    params(2) = 'omega_cdm'
    params(3) = 'n_s'
    params(4) = 'A_s'
    params(5) = 'H0'
    params(6) = 'm_ncdm'
    params(7) = 'MG_beta2'
    params(8) = 'omB'
    params(9) = 'omCDM'
    params(10) = 'ns'
    params(11) = 'As'
    params(12) = 'H0'
    params(13) = 'mncdm'
    params(14) = 'kappar'
    AsF = 1.d9*param_A_s
    write(omBfid,'(f7.5)') param_omega_b(2)
    write(omCDMfid,'(f6.4)') param_omega_cdm(2)
    write(nsfid,'(f6.4)') param_n_s(2)
    write(Asfid,'(f7.5)') AsF(2)
    write(H0fid,'(f5.2)') param_H0(2)
    write(mncdmfid,'(f5.3)') param_m_ncdm(2)
    write(MG_beta2fid,'(f4.2)') param_MG_beta2(2)

    Do m=0,1
        Do p=0,4
            ! omega_b 
            write(string_omega_b,'(f7.5)') param_omega_b(p)
            write(somb,fmt) param_omega_b(p)
            If (m .eq. 0) then
                inquire(file='./data/cl_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//'.Nbin10_DrdL.dat',exist=exist)
                If (exist .and. (p .ne. 2)) then
                    call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(1))//&
                    '_'//trim(somb)//'_lensing_cl.dat')
                else if ((p .eq. 2) .and. exist) then
                    call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat ./data/fiducial_euclid_galaxy_cl.dat')
                end if
                inquire(file='./data/el_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                '.fth_0.10.Nbin10_DrdL.dat',exist=exist2)
                If ((p .eq. 2) .and. exist2) then
                    call system ('mv ./data/el_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.fth_0.10.Nbin10_DrdL.dat ./data/El_fiducial_euclid_galaxy_cl.dat')
                end if
            else 
                inquire(file='./data/cl_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                '.Nbin10_Drd.dat',exist=exist)
                If (exist .and. (p .ne. 2)) then
                    call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(1))//&
                    '_'//trim(somb)//'_cl.dat')
                else if ((p .eq. 2) .and. exist) then
                    call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(string_omega_b)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat ./data/fiducial_euclid_galaxy_no_lensing_cl.dat')
                end if
            End if
            If (p .ne. 2) then
                ! omega_cdm
                write(string_omega_cdm,'(f6.4)') param_omega_cdm(p)
                write(somc,fmt) param_omega_cdm(p)
                If (m .eq. 0) then 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(string_omega_cdm)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat',exist=exist)
                    If (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                        //trim(params(9))//'_'//trim(string_omega_cdm)//'.'//trim(params(10))//'_'&
                        //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                        //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                        //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                        '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(2))//&
                        '_'//trim(somc)//'_lensing_cl.dat')
                    end if
                else 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(string_omega_cdm)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat',exist=exist)
                    If (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                        //trim(params(9))//'_'//trim(string_omega_cdm)//'.'//trim(params(10))//'_'&
                        //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                        //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                        //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                        '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(2))//&
                        '_'//trim(somc)//'_cl.dat')
                    end if
                end if
                ! n_s
                write(string_n_s,'(f6.4)') param_n_s(p)
                write(sns,fmt) param_n_s(p)
                If (m .eq. 0) then 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(string_n_s)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat',exist=exist)
                    If (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                        //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                        //trim(string_n_s)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                        //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                        //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                        '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(3))//&
                        '_'//trim(sns)//'_lensing_cl.dat') 
                    end if
                else 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(string_n_s)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat',exist=exist)
                    if (exist) then
                        call system('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(string_n_s)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(3))//&
                    '_'//trim(sns)//'_cl.dat')
                    end if
                end if
                ! A_s
                write(string_A_s,'(f7.5)') AsF(p)
                write(sAs,fmt) param_A_s(p)
                If (m .eq. 0) then 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(string_A_s)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(string_A_s)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(4))//&
                    '_'//trim(sAs)//'_lensing_cl.dat') 
                    end if
                else 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(string_A_s)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(string_A_s)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(4))//&
                    '_'//trim(sAs)//'_cl.dat') 
                    end if
                end if
                ! H0
                write(string_H0,'(f5.2)') param_H0(p)
                write(sH0,fmt) param_H0(p)
                If (m .eq. 0) then 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(string_H0)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(string_H0)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(5))//&
                    '_'//trim(sH0)//'_lensing_cl.dat') 
                    end if 
                else 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(string_H0)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(string_H0)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(5))//&
                    '_'//trim(sH0)//'_cl.dat') 
                    end if
                end if
                ! m_ncdm
                write(string_m_ncdm,'(f5.3)') param_m_ncdm(p)
                write(sm,fmt) param_m_ncdm(p)
                If (m .eq. 0) then
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(string_m_ncdm)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(string_m_ncdm)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(6))//&
                    '_'//trim(sm)//'_lensing_cl.dat') 
                    end if
                else 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(string_m_ncdm)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(string_m_ncdm)//'.'//trim(params(14))//'_'//trim(MG_beta2fid)//&
                    '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(6))//&
                    '_'//trim(sm)//'_cl.dat') 
                    end if
                end if
                ! MG_beta2
                write(string_MG_beta2,'(f4.2)') param_MG_beta2(p)
                write(sMG_beta2,fmt) param_MG_beta2(p)
                If (m .eq. 0) then
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(string_MG_beta2)//&
                    '.Nbin10_DrdL.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                        //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                        //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                        //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                        //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(string_MG_beta2)//&
                        '.Nbin10_DrdL.dat ./data/Cl_euclid_galaxy_'//trim(params(7))//&
                        '_'//trim(sMG_beta2)//'_lensing_cl.dat') 
                    end if
                else 
                    inquire(file='./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                    //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                    //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                    //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                    //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(string_MG_beta2)//&
                    '.Nbin10_Drd.dat',exist=exist)
                    if (exist) then
                        call system ('mv ./data/cl_'//trim(params(8))//'_'//trim(omBfid)//'.'&
                        //trim(params(9))//'_'//trim(omCDMfid)//'.'//trim(params(10))//'_'&
                        //trim(nsfid)//'.'//trim(params(11))//'_'//trim(Asfid)//'e-9.'&
                        //trim(params(12))//'_'//trim(H0fid)//'.'//trim(params(13))//'_'&
                        //trim(mncdmfid)//'.'//trim(params(14))//'_'//trim(string_MG_beta2)//&
                        '.Nbin10_Drd.dat ./data/Cl_euclid_galaxy_'//trim(params(7))//&
                        '_'//trim(sMG_beta2)//'_cl.dat') 
                    end if
                end if
            End if
        End Do
    End Do
end subroutine change_filename_format

end module functions

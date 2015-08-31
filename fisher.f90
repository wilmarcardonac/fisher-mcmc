Program fisher 
!#############################################
!#############################################
! Here we load all the modules we need
!#############################################
!#############################################

use fiducial
use arrays
use functions 

!################################
!################################
! We declare variables to be used
!################################
!################################

Implicit none
Integer*4,parameter :: p = (n_points-1)/2       ! index to count sigma values
Integer*4 :: m,n,i,j,q    ! integers for small loops
Integer*4 :: seed1,seed2                        ! seeds needed by random number generator 
character(len=15) :: filetype                   ! auxiliary variable for fisher analysis
character*16 :: ElCl                            ! allows to choose either error or Cl file 
character*16 :: phrase                          ! phrase needed by number random generator  
Real*8 :: old_loglikelihood,current_loglikelihood      ! likelihood values
Real*4 :: genunf,gennor                                ! random uniform deviates 
Real*4,dimension(number_of_parameters*(number_of_parameters+3)/2 + 1) :: parm ! array needed by random number generator
Real*4,dimension(number_of_parameters) :: work,x_old,x_new  ! array needed by random number generator 
logical :: lensing,cl_file_exist,ini_file_exist,start_from_fiducial,testing_Gaussian_likelihood  ! explanation below in assignments
logical :: c1,c2,c4,c5,c6,c7,non_plausible_parameters ! control plausible values of cosmological parameters
logical :: using_inverse_fisher_matrix                ! MCMC parameter
Real*8,dimension(number_of_parameters,number_of_parameters) :: Covgauss ! Covariance matrix of Gaussian likelihood
Real*8,dimension(number_of_parameters,number_of_parameters) :: Covguess ! Covariance matrix 
Real*8 :: jumping_factor    ! jumping factor for MCMC (increase if  you want bigger step size, decrease otherwise)
Real*4 :: average_acceptance_probability
Real*8 :: random_uniform    ! Random uniform deviate between o and 1
Integer*4 :: number_accepted_points,number_rejected_points ! MCMC parameters 
Integer*4 :: weight    ! It counts the number of steps taken before moving to a new point in MCMC 
logical :: compute_data_fisher_analysis    !    Fisher matrix analysis parameter
logical :: do_only_mcmc_analysis    !    Do only MCMC analysis ?  
    
!##########################################################
!##########################################################
! Assignments and initialization of random number generator 
!##########################################################
!##########################################################

open(15,file='./output/execution_information.txt')

weight = 1

start_from_fiducial = .false.                    ! starting MCMC analysis from fiducial point if true

testing_Gaussian_likelihood = .false.           ! If testing Gaussian likelihood

compute_data_fisher_analysis = .false. 

do_only_mcmc_analysis = .true.

If (testing_Gaussian_likelihood) then

    jumping_factor = 2.38d0/sqrt(dble(number_of_parameters))    ! Modify according to wanted initial acceptance probability

Else

    jumping_factor = 2.38d0/sqrt(dble(number_of_parameters))*1.d-4

End If

using_inverse_fisher_matrix = .false.           ! True if one wants to use inverse of Fisher matrix as a covariance matrix 

number_rejected_points = 0

number_accepted_points = 0

lensing = .false.                               ! not considering lensing for MCMC runs if false

phrase = 'randomizer'                           ! random number generator initializer

call initialize()                               ! initialize random number generators

call phrtsd(phrase,seed1,seed2)                 ! generate seeds for random numbers from phrase

call set_initial_seed(seed1,seed2)              ! set initial seeds for random number generator 

If (testing_Gaussian_likelihood) then

    Do m=1,number_of_parameters                    ! Set covariance matrix when testing Gaussian 
        Do n=1,number_of_parameters                !
            If (m .eq. n) then                     ! likelihood 
                Covgauss(m,n) = 1.d0
            else 
                Covgauss(m,n) = 0.d0
            End If
        End Do
    End Do

Go to 1

End If

If (.not. using_inverse_fisher_matrix) then        ! Set covariance matrix

    Covguess = 0.d0

    Covguess(1,1) = sigma_omega_b**2 

    Covguess(2,2) = sigma_omega_cdm**2

    Covguess(3,3)= sigma_n_s**2

    Covguess(4,4) = (sigma_A_s/A_s)**2

    Covguess(5,5) = sigma_H0**2

    Covguess(6,6) = sigma_m_ncdm**2

    Covguess(7,7) = sigma_MG_beta2**2

End If

!######################################################################################
!######################################################################################
!                          FISHER MATRIX ANALYSIS STARTS HERE
!######################################################################################
!######################################################################################

write(15,*) 'FISHER matrix analysis has started '

!######################################################################################
! Allocate memory for grid of models to be computed, covariance matrix, and Cl's arrays 
!###################################################################################### 

allocate (param_omega_b(0:n_points-1), param_omega_cdm(0:n_points-1), param_n_s(0:n_points-1), stat = status1)
allocate (param_A_s(0:n_points-1), param_H0(0:n_points-1), param_m_ncdm(0:n_points-1),&
param_MG_beta2(0:n_points-1), stat = status2)

!##################################################################
! Filling arrays of cosmological parameters, creating ini files and
! computing data (if required)
!##################################################################

call fill_parameters_array(p)

If (compute_data_fisher_analysis) then

    write(15,*) 'Computing data for Fisher matrix analysis '

    call compute_data_for_fisher_analysis(p)

    write(15,*) 'Data for Fisher matrix analysis have been computed '

Else if (do_only_mcmc_analysis) then

    write(15,*) 'Computing data for MCMC  analysis '

    call run_class('omega_b',omega_b,.true.,.true.)

    call run_class('omega_b',omega_b,.true.,.false.)

    write(15,*) 'Data for MCMC analysis have been computed '

Else

    write(15,*) 'Using existing data for Fisher matrix analysis '

End If

!###############################################
! Changing file names to match Francesco's files
!###############################################

!call change_filename_format()

!################################################
! Reading fiducial data files into fortran arrays
!################################################

allocate (El(lmin:lmax,0:nbins,0:nbins),Cl_fid(lmin:lmax,0:nbins,0:nbins),Cl_fid_nl(lmin:lmax,0:nbins,0:nbins),&
Cl_obs(lmin:lmax,0:nbins,0:nbins), Nl(1:nbins,1:nbins),Cl_syst(lmin:lmax,0:nbins,0:nbins),stat = status3)

write(15,*) 'Reading data of fiducial model with/without lensing and error data '

call read_data(Cl_fid,10,filetype,ElCl,.true.,.true.,.true.)

call read_data(El,10,filetype,ElCl,.true.,.true.,.false.)

call read_data(Cl_fid_nl,10,filetype,ElCl,.false.,.true.,.true.)

!###########################
! Computing systematic error
!###########################

call compute_systematic_error()
 
call write_Cl_syst(Cl_syst,11)

!#####################
! Computing shot noise
!#####################

call compute_shot_noise()

!########################
! Computing observed Cl's
!########################

call compute_observed_Cl()

write(15,*) '-ln(L/L_{max}) at the fiducial point without lensing is ', -euclid_galaxy_cl_likelihood(Cl_fid_nl)

write(15,*), '-ln(L/L_{max}) at the fiducial point with lensing is ', -euclid_galaxy_cl_likelihood(Cl_fid)

If (.not.do_only_mcmc_analysis) then

    !############################
    ! Computing covariance matrix 
    !############################

    allocate (cov(lmin:lmax,1:nbins,1:nbins,1:nbins,1:nbins),stat = status4)

    call compute_covariance_matrix()

    allocate (cov_l_IP(lmin:lmax,1:nbins*(nbins+1)/2,1:nbins*(nbins+1)/2),&
    inv_cov_l_IP(lmin:lmax,1:nbins*(nbins+1)/2,1:nbins*(nbins+1)/2),&
    cov_l_IP_oa(lmin:lmax,1:nbins,1:nbins),inv_cov_l_IP_oa(lmin:lmax,1:nbins,1:nbins),stat = status6)

    !###################################################
    ! Writing covariance matrix in terms of superindices
    !###################################################

    call write_cov_two_indices()

    call write_cov_two_indices_oa()

    !call write_covariance_matrix(2)

    !call write_covariance_matrix(200)

    !call write_covariance_matrix(2000)

    deallocate (cov)

    !#######################################
    ! Computing inverse of covariance matrix
    !#######################################

    call inverting_matrix()

    call inverting_matrix_oa()

    !call write_inverse_covariance_matrix(2)

    !call write_inverse_covariance_matrix(200)

    !call write_inverse_covariance_matrix(2000)

    deallocate (cov_l_IP,cov_l_IP_oa)

    !##############################################################
    ! Writing inverse covariance matrix in terms of indices i,j,p,q
    !##############################################################

    allocate (inv_cov(lmin:lmax,1:nbins,1:nbins,1:nbins,1:nbins),inv_cov_oa(lmin:lmax,1:nbins,1:nbins,1:nbins,1:nbins))

    call write_inv_cov_four_indices()

    call write_inv_cov_four_indices_oa() 

    deallocate (inv_cov_l_IP,inv_cov_l_IP_oa)

    !######################################################################
    ! Computing derivatives of Cl's w.r.t different cosmological parameters
    !######################################################################

    allocate (Cl_1(lmin:lmax,0:nbins,0:nbins),Cl_2(lmin:lmax,0:nbins,0:nbins),Cl_3(lmin:lmax,0:nbins,0:nbins),&
    Cl_4(lmin:lmax,0:nbins,0:nbins),dCl(lmin:lmax,0:nbins,0:nbins),Cl_5(lmin:lmax,0:nbins,0:nbins),&
    Cl_6(lmin:lmax,0:nbins,0:nbins),Cl_7(lmin:lmax,0:nbins,0:nbins),Cl_8(lmin:lmax,0:nbins,0:nbins),&
    dCl_nl(lmin:lmax,0:nbins,0:nbins),stat = status5)

    call compute_derivatives()

    deallocate (Cl_1,Cl_2,Cl_3,Cl_4,Cl_5,Cl_6,Cl_7,Cl_8,dCl,dCl_nl)

    deallocate (param_omega_b, param_omega_cdm, param_n_s, param_A_s, param_H0, param_m_ncdm,param_MG_beta2)

    !########################
    ! Computing Fisher matrix
    !######################## 

    allocate (d1(lmin:lmax,0:nbins,0:nbins),d2(lmin:lmax,0:nbins,0:nbins),d3(lmin:lmax,0:nbins,0:nbins),&
    d4(lmin:lmax,0:nbins,0:nbins),d5(lmin:lmax,0:nbins,0:nbins),d6(lmin:lmax,0:nbins,0:nbins),&
    F_ab(1:number_of_parameters,1:number_of_parameters),F_ab_nl(1:number_of_parameters,1:number_of_parameters),&
    inv_F_ab(1:number_of_parameters,1:number_of_parameters),B_beta(1:number_of_parameters),&
    b_lambda(1:number_of_parameters),d7(lmin:lmax,0:nbins,0:nbins),stat = status1)

    call compute_fisher_matrix(.true.,.false.) ! first logical variable -> true if including lensing
                                           ! second logical variable -> true if including only auto-correlations
    call compute_inverse_fisher_matrix()

    call compute_fisher_matrix(.false.,.false.)

    call compute_fisher_matrix(.true.,.true.)

    call compute_fisher_matrix(.false.,.true.)

    !####################################
    ! Computing B_beta and b_lambda_alpha
    !####################################

    call compute_B_beta()

    call compute_b_lambda_alpha()

    !################################################
    ! Computing ratio of likelihood along bias vector
    !################################################

    allocate (Cl_current(lmin:lmax,0:nbins,0:nbins),stat = status3)

    call compute_ratio_likelihood()

    deallocate(Cl_fid,Cl_fid_nl,Cl_current)

    deallocate (d1,d2,d3,d4,d5,d6,d7,F_ab,F_ab_nl,inv_cov,inv_cov_oa,Cl_syst,B_beta,b_lambda)

    write(15,*) 'FISHER matrix analysis has ended '

End If


!########################################################################################
!########################################################################################
!                                  FISHER ANALYSIS ENDS HERE
!########################################################################################
!########################################################################################

!########################################################################################
!########################################################################################
!                   MARKOV CHAIN MONTE CARLO ANALYSIS STARTS HERE 
! 
! We will use the fiducial model computed above for the Fisher matrix analysis in order 
! to compare results of the two methods afterwards. The observed Cl's include El files 
! and shot noise, Nl.
!########################################################################################
!########################################################################################

write(15,*) 'Starting MCMC analysis '

!########################################################################################
! Allocate memory for both old and current points in parameter space. Also for current Cl
!########################################################################################

1 If (testing_Gaussian_likelihood) then
      write(15,*) 'Testing MCMC analysis with Gaussian likelihood'
  End If

allocate (old_point(1:number_of_parameters),current_point(1:number_of_parameters),stat = status1)

allocate (acceptance_probability(number_iterations),stat = status2)

allocate (Cl_current(lmin:lmax,0:nbins,0:nbins),stat = status3)

! #####################################################################################################
! First, we generate a random point in parameter space (same parameter space as in the Fisher analysis,
! except for A_s because the version of CLASS we will use does not take ln(10^10 A_s) as an input). 
! Random number generators work with single precision whereas our function use double; we change it.
! #####################################################################################################

If (testing_Gaussian_likelihood) then
    Go to 2
End If

If (start_from_fiducial) then

    old_point(1) = omega_b
    old_point(2) = omega_cdm
    old_point(3) = n_s
    old_point(4) = A_s
    old_point(5) = H0
    old_point(6) = m_ncdm
    old_point(7) = MG_beta2
    
    Do m=1,number_of_parameters

        If (m .eq. 4) then

            x_old(m) = real(log(1.d1**1.d1*old_point(m)))

        else

            x_old(m) = real(old_point(m))

        End If

    End Do

Else

    x_old(1) = genunf(real(omega_b-sigma_omega_b),real(omega_b+sigma_omega_b))         ! omega_b
    x_old(2) = genunf(real(omega_cdm-sigma_omega_cdm),real(omega_cdm+sigma_omega_cdm)) ! omega_cdm
    x_old(3) = genunf(real(n_s-sigma_n_s),real(n_s+sigma_n_s))                         ! n_s
    x_old(4) = log((1.d1**10)*genunf(real(A_s-sigma_A_s),real(A_s+sigma_A_s)))         ! log(10^10*A_s)
    x_old(5) = genunf(real(H0-sigma_H0),real(H0+sigma_H0))                             ! H0
    x_old(6) = genunf(real(m_ncdm-sigma_m_ncdm),real(m_ncdm+sigma_m_ncdm))             ! m_ncdm
    x_old(7) = genunf(real(MG_beta2-sigma_MG_beta2),real(MG_beta2+sigma_MG_beta2))     ! MG_beta2

    Do m=1,number_of_parameters
        If (m == 4) then
            old_point(m) = exp(dble(x_old(m)))/(1.d1**1.d1)
        else
            old_point(m) = dble(x_old(m))
        End If
    End Do

End If

!#############################################################################
! Second, we write ini file corresponding to current point in parameter space.  
!#############################################################################

inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist) 
inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

If (cl_file_exist) then

    call system('rm ./output/current_euclid_galaxy_cl.dat')

End If 

If (ini_file_exist) then

    call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')

End If

call write_ini_file(old_point(1),old_point(2),old_point(3),old_point(4),old_point(5),old_point(6),&
                    old_point(7),tau,N_ur,N_ncdm,deg_ncdm,lensing)

!###############################################
! Run CLASS for current point in parameter space  
!###############################################

inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist) 

If (ini_file_exist) then

    call run_current_model(lensing) ! Remember, lensing flag allows to run with/without lensing
 
End If

!######################################################################################################
! Check existence of data file for current model in parameter space. If data exists, fill array of Cl in
! and compute likelihood (fill old_loglikelihood variable in)
!######################################################################################################

If (ini_file_exist) then

    inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

    If (cl_file_exist) then

        call read_Cl(Cl_current,11,lensing)

        old_loglikelihood = euclid_galaxy_cl_likelihood(Cl_current)

        call system('rm ./output/current_euclid_galaxy_cl.dat')    ! Remove Cl file

    Else

        write(15,*) 'Something went wrong with CLASS for current model. Unlikely likelihood assigned'

        old_loglikelihood = -1.d10

    End If

    call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')    ! Remove ini file

Else

    write(15,*) 'Not ini file for current model. Something went wrong with subroutine "write_ini_file" '

    write(15,*) 'Unlikely log likelihood assigned to current point '
    
    old_loglikelihood = -1.d10

End if

!#########################################
! Open data file to store MCMC computation 
!#########################################

2 open(13,file='./output/mcmc_final_output.txt')

open(14,file='./output/mcmc_output.txt')    !    Temporary file 

write(13,*) '# Number of iterations in MCMC : ', number_iterations - steps_taken_before_definite_run

If (start_from_fiducial .and. (.not.testing_Gaussian_likelihood)) then

    write(13,*) '# Fiducial model is (parameters ordered as below) :', omega_b, omega_cdm, n_s, A_s, H0, m_ncdm, MG_beta2

    write(13,*) '# ln(L/L_max) at the fiducial model :', old_loglikelihood

End If

write(13,*) '# Weight   -ln(L/L_{max})    omega_b    omega_cdm    n_s    A_s    H0    m_ncdm    MG_beta2 ' 

open(16,file='./output/mcmc_final_output.paramnames')    !    File with names of parameters needed by Getdist

write(16,*) 'omega_b    \omega_b'

write(16,*) 'omega_cdm    \omega_{cdm}'

write(16,*) 'n_s    n_s'

write(16,*) 'A_s    A_s'

write(16,*) 'H0    H_0'

write(16,*) 'm_ncdm    m_{ncdm}'

write(16,*) 'MG_beta2    \beta_2'

close(16)

If (testing_Gaussian_likelihood) then 

    open(17,file='./output/mcmc_final_output.ranges')    !    File with hard bounds needed by Getdist

    write(17,*) 'omega_b    N    N '

    write(17,*) 'omega_cdm    N    N '

    write(17,*) 'n_s    N    N '

    write(17,*) 'A_s    N    N '

    write(17,*) 'H0   N    N '

    write(17,*) 'm_ncdm    N    N '

    write(17,*) 'MG_beta2    N    N ' 

    close(17)

Else

    open(17,file='./output/mcmc_final_output.ranges')    !    File with hard bounds needed by Getdist

    write(17,*) 'omega_b    0    N '

    write(17,*) 'omega_cdm    0    N '

    write(17,*) 'n_s    N    N '

    write(17,*) 'A_s    0    3.e-9 '

    write(17,*) 'H0   0    85 '

    write(17,*) 'm_ncdm    0    N '

    write(17,*) 'MG_beta2    0    N'

    close(17)

End If

!############################################
! Loop to explore parameter space starts here
!############################################

If (testing_Gaussian_likelihood) then        ! Used if testing Gaussian likelihood

    Do i=1,number_of_parameters
        x_old(i) = genunf(-1.,1.)
    End Do

    Do m=1,number_of_parameters
    
        If (m == 4) then
            old_point(m) = dble(x_old(m)) !exp(dble(x_old(m)))/(1.d1**1.d1)
        else
            old_point(m) = dble(x_old(m))
        End If

    End Do

    old_loglikelihood = log_Gaussian_likelihood(old_point)

    !5 call read_bestfit_mcmc(old_point)

End If

Do m=1,number_iterations

    !######################################################################################################
    ! Generate new point in parameter space from a multivariate normal distribution and the covariance 
    ! matrix computed out of the Fisher matrix  analysis above. We use RANLIB library. Be careful with 
    ! x_old and old_point definitions 
    !######################################################################################################

    Do q=1,number_iterations 

        If (testing_Gaussian_likelihood) then

            call setgmn(x_old,real(jumping_factor*Covgauss),number_of_parameters,parm) ! used if testing Gaussian likelihood
 
            call genmn(parm,x_new,work)

            exit

        else

            If (using_inverse_fisher_matrix) then

                call setgmn(x_old,real(jumping_factor*inv_F_ab),number_of_parameters,parm) 

                call genmn(parm,x_new,work)

            else

                call setgmn(x_old,real(jumping_factor*Covguess),number_of_parameters,parm) 

                call genmn(parm,x_new,work)

            End If 

        End If

        c1 = x_new(1) .lt. real(0.d0)
        c2 = x_new(2) .lt. real(0.d0)
        c4 = (x_new(4) .lt. real(0.d0)).or.(x_new(4) .gt. real(log(30.d0))) ! limit As<3.d-9 but using log(10^10As)
        c5 = (x_new(5) .lt. real(0.d0)).or.(x_new(5).gt.real(85.d0))
        c6 = x_new(6) .lt. real(0.d0)
        c7 = x_new(7) .le. real(0.d0)
        non_plausible_parameters = ((c1 .or. c2) .or. (c4 .or. c5)) .or. (c6 .or. c7) 

        If (non_plausible_parameters .and. (q .ne. number_iterations)) then

            call genmn(parm,x_new,work)

        else if (q .eq. number_iterations) then

            write(15,*) 'Loop to generate multivariate Gaussian deviate hit maximum number of iterations '

            stop

        else 

            exit

        End If

    End Do
    
    Do n=1,number_of_parameters

        If (n .eq. 4) then

            If (testing_Gaussian_likelihood) then

                current_point(n) = dble(x_new(n)) ! used if testing Gaussian likelihood

            else

                current_point(n) = dexp(dble(x_new(n)))/(1.d1**1.d1) ! Converting log(10**10A_s) to A_s 

            End If

        else

            current_point(n) = dble(x_new(n))

        End If

    End Do

    If (testing_Gaussian_likelihood) then

        Go to 3        ! used if testing Gaussian likelihood

    End If

    !####################################################
    ! Write ini file for current point in parameter space 
    !####################################################

    call write_ini_file(current_point(1),current_point(2),current_point(3),current_point(4),&
    current_point(5),current_point(6),current_point(7),tau,N_ur,N_ncdm,deg_ncdm,lensing)

    !################################
    ! Call CLASS for current ini file 
    !################################

    inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist)

    If (ini_file_exist) then

        call run_current_model(lensing) ! Look at lensing flag declaration above

    End If    

    !######################################################################################################
    ! Check existence of data for current point in parameter space and fill array of Cl in if file exists
    !######################################################################################################

    If (ini_file_exist) then 

        inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

        If (cl_file_exist) then

            call read_Cl(Cl_current,11,lensing)

            current_loglikelihood = euclid_galaxy_cl_likelihood(Cl_current)

            call system('rm ./output/current_euclid_galaxy_cl.dat')    ! Remove Cl file

        Else

            write(15,*) 'Cl file was not created, something went wrong with CLASS for current point'
            write(15,*) 'Assigning unlikely value of log likelihood to current point'

            current_loglikelihood = -1.d10

        End if

        call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')    ! Remove ini file 

    Else

        write(15,*) 'Not ini file for current model. Something went wrong with subroutine "write_ini_file" '

        write(15,*) 'Unlikely log likelihood assigned to current point '
    
        current_loglikelihood = -1.d10

    End If

    3 If (testing_Gaussian_likelihood) then 
    
          current_loglikelihood = log_Gaussian_likelihood(current_point) ! used if testing Gaussian likelihood

      End If

    !######################################################################################################
    ! Decide whether or not the current_point in parameter space becomes old_point in parameter space 
    !######################################################################################################

    If (current_loglikelihood .ge. old_loglikelihood) then ! It accepts the current point

        number_accepted_points = number_accepted_points + 1    ! Used to compute acceptance rate

        acceptance_probability(m) = min(1.d0,dexp(current_loglikelihood - old_loglikelihood))    

        If (m .le. steps_taken_before_definite_run) then

            write(14,*) weight,-old_loglikelihood,old_point(1),old_point(2),&
            old_point(3),old_point(4),old_point(5),old_point(6),old_point(7)

        else

            write(13,*) weight,-old_loglikelihood,old_point(1),old_point(2),&
            old_point(3),old_point(4),old_point(5),old_point(6),old_point(7)

        End If

        weight = 1    

        old_loglikelihood = current_loglikelihood
        
        Do i=1,number_of_parameters 

            old_point(i) = current_point(i)

            If (i .eq. 4) then

                If (testing_Gaussian_likelihood) then

                    x_old(i) = real(old_point(i)) ! used if testing Gaussian likelihood

                else

                    x_old(i) = real(log(1.d1**1.d1*old_point(i))) ! converting A_s to log(10**10*A_s)

                End If 

            else

                x_old(i) = real(old_point(i))

            End If

        End Do
   
    else 

        random_uniform = dble(genunf(real(0.),real(1.)))
 
        If ( random_uniform .le. dexp(current_loglikelihood-old_loglikelihood)) then 
            ! It accetps the current point 

            number_accepted_points = number_accepted_points + 1    ! Used to compute acceptance rate

            acceptance_probability(m) = min(1.d0,dexp(current_loglikelihood - old_loglikelihood))    

            If (m .le. steps_taken_before_definite_run) then

                write(14,*) weight,-old_loglikelihood,old_point(1),old_point(2),&
                old_point(3),old_point(4),old_point(5),old_point(6),old_point(7)

            else

                write(13,*) weight,-old_loglikelihood,old_point(1),old_point(2),&
                old_point(3),old_point(4),old_point(5),old_point(6),old_point(7)

            End If

            weight = 1

            old_loglikelihood = current_loglikelihood

            Do i=1,number_of_parameters 

                old_point(i) = current_point(i)

                If (i .eq. 4) then

                    If (testing_Gaussian_likelihood) then

                        x_old(i) = real(old_point(i)) ! used when testing Gaussian likelihood

                    Else

                        x_old(i) = real(log(1.d1**1.d1*old_point(i))) ! converting A_s to log(10**10*A_s)

                    End If

                else

                    x_old(i) = real(old_point(i))

                End If

            End Do

        else   ! The code rejects the current point 

            If (m .gt. steps_taken_before_definite_run) then

                number_rejected_points = number_rejected_points + 1            

            End If

            acceptance_probability(m) = min(1.d0,dexp(current_loglikelihood - old_loglikelihood))    

            weight = weight + 1

            Do i=1,number_of_parameters 

                If (i .eq. 4) then

                    If (testing_Gaussian_likelihood) then

                        x_old(i) = real(old_point(i)) ! Used if testing Gaussian likelihood

                    Else

                        x_old(i) = real(log(1.d1**1.d1*old_point(i))) ! convert A_s to log(10**10*A_s)

                    End If

                else

                    x_old(i) = real(old_point(i))

                End If

            End Do

        End If

    End If

    !###################################################################################################
    ! Compute average acceptance probability and update covariance matrix and jumping factor (if needed)
    !###################################################################################################

    If ((mod(m,jumping_factor_update) .eq. 0) .and. (m .le. steps_taken_before_definite_run) ) then

        average_acceptance_probability = sum(acceptance_probability(m-jumping_factor_update+1:m))&
        /real(jumping_factor_update)

        write(15,*) 'Current average acceptance probability ',average_acceptance_probability
        
        If (average_acceptance_probability .lt. 0.1) then 

            jumping_factor = jumping_factor*(1.d0 - step_size_changes)    !    Decreasing step size

        Else if (average_acceptance_probability .gt. 0.4) then

            jumping_factor = jumping_factor*(1.d0 + step_size_changes)    !    Increasing step size 

        End If

        If (testing_Gaussian_likelihood) then

            If ( mod(m,covariance_matrix_update) .eq. 0 ) then

                call system('cd output; python compute_covariance_matrix_Gaussian.py')
     
                close(14)

                call system('rm ./output/mcmc_output.txt')

!                call read_covariance_matrix_mcmc(Covgauss)

                write(15,*) 'Iteration ', m

                write(15,*) 'Current covariance matrix ', Covgauss

                open(14,file='./output/mcmc_output.txt')

            End If

        Else if (.not. using_inverse_fisher_matrix) then  

            If ( mod(m,covariance_matrix_update) .eq. 0 ) then

                call system('cd output; python compute_covariance_matrix.py')
     
                close(14)

                call system('rm ./output/mcmc_output.txt')

!                call read_covariance_matrix_mcmc(Covguess)

                write(15,*) 'Iteration ', m

                write(15,*) 'Current covariance matrix ', Covguess

                open(14,file='./output/mcmc_output.txt')

            End If

        Else If (using_inverse_fisher_matrix) then

            If ( mod(m,covariance_matrix_update) .eq. 0 ) then

                call system('cd output; python compute_covariance_matrix.py')
     
                close(14)

                call system('rm ./output/mcmc_output.txt')

!                call read_covariance_matrix_mcmc(inv_F_ab)

                write(15,*) 'Iteration ', m

                write(15,*) 'Current covariance matrix ', inv_F_ab

                open(14,file='./output/mcmc_output.txt')

            End If

        End If

    End If

    !#########################################
    ! Loop to sample parameter space ends here
    !#########################################

End Do

!############################################
! Write last informations and close data file 
!############################################

write(15,*) 'Number of rejected points: ', number_rejected_points

write(15,*) 'Acceptance ratio ', dble(number_iterations - steps_taken_before_definite_run - number_rejected_points)/&
dble(number_iterations - steps_taken_before_definite_run)

close(13)

close(14)

close(15)

If (.not. testing_Gaussian_likelihood) then

    deallocate (old_point,current_point,inv_F_ab,Cl_current,Nl,El,Cl_obs,acceptance_probability)

End If
!call system('cd output; python compute_covmatrix_bestfit.py')

End Program fisher





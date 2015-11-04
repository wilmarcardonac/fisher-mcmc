Program fisher 

  !####################
  ! LOAD NEEDED MODULES
  !####################

  use fiducial
  use arrays
  use functions 

  !#####################
  ! VARIABLES DECLARATION
  !######################

  Implicit none

  Integer*4                              :: m,n,i,q    ! COUNTERS FOR LOOPS

  !#################
  ! FISHER VARIABLES
  !#################

  Integer*4,parameter                    :: p = (n_points-1)/2 ! COUNT SIGMA VALUES IN COSMOLOGICAL PARAMETERS

  Real*4,dimension(number_of_parameters) :: work,x_old,x_new   ! ARRAYS NEEDED BY RANDOM NUMBER GENERATOR

  Character(len=15)                      :: filetype     ! AUXILIARY VARIABLE 
  Character*16                           :: ElCl         ! ALLOWS TO CHOOSE EITHER EL OR CL

  !###############
  ! MCMC VARIABLES
  !###############

  Integer*4                              :: seed1,seed2 ! SEEDS NEEDED BY RANDOM NUMBER GENERATOR
  Integer*4                              :: number_accepted_points,number_rejected_points ! COUNT POINTS IN PARAMETER SPACE
  Integer*4                              :: weight    ! COUNTS NUMBER OF TAKEN STEPS BEFORE MOVING TO A NEW POINT
  Integer*4                              :: job_number ! JOB IDENTIFIER WHEN RUNNING MULTIPLE CHAINS

  Real*8                                 :: old_loglikelihood,current_loglikelihood      ! STORE LIKELIHOOD VALUES
  Real*4                                 :: genunf                                ! RANDOM UNIFORM DEVIATES
  Real*4,dimension(number_of_parameters*(number_of_parameters+3)/2 + 1) :: parm ! ARRAY NEEDED BY RANDOM NUMBER GENERATOR
  Real*8,dimension(number_of_parameters,number_of_parameters)           :: Covgauss ! COVARIANCE MATRIX OF GAUSSIAN LIKELIHOOD
  Real*8,dimension(number_of_parameters,number_of_parameters)           :: Covguess ! COVARIANCE MATRIX 
  Real*8                                 :: jumping_factor    ! STORES JUMPING FACTOR (INCREASE IF BIGGER STEP SIZE WANTED, DECREASE OTHERWISE)
  Real*4                                 :: average_acceptance_probability
  Real*8                                 :: random_uniform    ! RANDOM UNIFORM DEVIATE BETWEEN 0 AND 1
  Real*8,dimension(number_of_parameters) :: bestfit,means                           ! SAVES BESTFIT AND MEANS OF PARAMETERS 

  Logical                                :: cl_file_exist,ini_file_exist,exe_file ! CHECK EXISTENCE OF FILES
  Logical,parameter                      :: lensing = .false. ! CONSIDER LENSING TERMS IN MCMC RUNS IF SET IT TRUE
  Logical                                :: not_good_app,non_plausible_parameters ! CONTROL PLAUSIBLE VALUES OF COSMOLOGICAL PARAMETERS
  Logical,dimension(number_of_parameters) :: plausibility  

  Character(len=10) :: string ! STORES STRINGS FOR INTEGERS

  !##########################################################
  ! ASSIGNMENTS AND INITIALIZATION OF RANDOM NUMBER GENERATOR
  !##########################################################

  If (multiple_chains) then

     Do m=1,number_of_parallel_jobs

        job_number = m + 20

        write(string,'(i2.2)') job_number 

        inquire(file=EXECUTION_INFORMATION_CHAIN//trim(string)//'.txt',exist=exe_file) 

        If (exe_file) then

           continue

        Else

           open(job_number,file=EXECUTION_INFORMATION_CHAIN//trim(string)//'.txt')

           exit
           
        End If

     End Do

  Else

     job_number = 15

     open(job_number,file=Execution_information)

  End If
  
  If (do_mcmc_analysis) then

     weight = 1

     number_rejected_points = 0

     number_accepted_points = 0

     write(job_number,*) 'SETTING UP RANDOM NUMBER GENERATOR'

     call initialize() ! INITIALIZE RANDOM NUMBER GENERATORS 

     call phrtsd(phrase,seed1,seed2) ! GENERATE SEEDS FOR RANDOM NUMBERS FROM PHRASE

     call set_initial_seed(seed1,seed2) ! SET INITIAL SEEDS FOR RANDOM NUMBER GENERATOR 

     If (testing_Gaussian_likelihood) then

        write(job_number,*) 'SETTING JUMPING FACTOR AND COVARIANCE MATRIX FOR GAUSSIAN LIKELIHOOD'

        jumping_factor = 2.38d0/sqrt(dble(number_of_parameters)) ! INCREASE/DECREASE ACCORDING TO WANTED INITIAL ACCEPTANCE PROBABILITY

        ! SETTING COVARIANCE MATRIX
        Do m=1,number_of_parameters  

           Do n=1,number_of_parameters 

              If (m .eq. n) then      

                 Covgauss(m,n) = 1.d0

              Else 

                 Covgauss(m,n) = 0.d0

              End If

           End Do

        End Do
        ! COVARIANCE MATRIX SET

     Else  

        Covguess = 0.d0

        write(job_number,*) 'SETTING JUMPING FACTOR AND COVARIANCE MATRIX'

        ! SETTING COVARIANCE MATRIX
        If (using_inverse_fisher_matrix) then
               
           write(job_number,*) 'USING INVERSE OF FISHER MATRIX AS A COVARIANCE MATRIX FOR MCMC ANALYSIS'

           call read_inverse_fisher_matrix(Covguess)

        Else

           If (read_covariance_matrix_from_file) then

              write(job_number,*) 'READING COVARIANCE MATRIX FOR MCMC ANALYSIS FROM FILE'

              call read_covariance_matrix_mcmc(Covguess)

           Else

              write(job_number,*) 'USING DIAGONAL MATRIX AS COVARIANCE MATRIX'

              Covguess(1,1) = sigma_omega_b**2 

              Covguess(2,2) = sigma_omega_cdm**2

              Covguess(3,3)= sigma_n_s**2

              Covguess(4,4) = (sigma_A_s/A_s)**2

              Covguess(5,5) = sigma_H0**2

              Covguess(6,6) = sigma_m_ncdm**2

              Covguess(7,7) = sigma_MG_beta2**2

           End If

        End If
        ! COVARIANCE MATRIX SET

        jumping_factor = 2.38d0/sqrt(dble(number_of_parameters))*1.d-2 !*1.d-4 ! INCREASE/DECREASE ACCORDING TO WANTED INITIAL ACCEPTANCE PROBABILITY

        ! COVARIANCE MATRIX ADJUSTED 
        Covguess = jumping_factor*Covguess

        ! COMPUTING FIDUCIAL SPECTRA CL AND EL (IF NEEDED)
        write(job_number,*) 'SUBMITTING JOBS TO COMPUTE DATA FOR FIDUCIAL MODEL (CL AND EL) IF NEEDED'
        write(job_number,*) 'IF NEW DATA WANTED, REMOVE CORRESPONDING FILES BEFORE RUNNING THE CODE'

        call run_class('omega_b',omega_b,.true.,.true.)

        call run_class('omega_b',omega_b,.true.,.false.)

     End If

  Else 

     write(job_number,*) 'NOT DOING MCMC ANALYSYS'

  End If

  !#######################
  ! FISHER MATRIX ANALYSIS
  !#######################

  If (do_fisher_analysis) then

     write(job_number,*) 'STARTING FISHER MATRIX ANALYSIS'

     If (testing_precision) then

        If (compute_data_testing_precision) then
               
           call compute_data_for_testing_precision()

           write(job_number,*) 'JOBS TO COMPUTE DATA FOR TESTING OPTIMAL PRECISION PARAMETERS HAVE BEEN SUBMITTED'

           stop

        Else

           write(job_number,*) 'USING EXISTING DATA FOR TESTING PRECISION'
                
           call testing_precision_cl()

           stop

        End If
     
     Else

        continue

     End If

     ! ALLOCATING MEMORY FOR GRID OF MODELS
     allocate (param_omega_b(0:n_points-1), param_omega_cdm(0:n_points-1), param_n_s(0:n_points-1),&
          param_A_s(0:n_points-1), param_H0(0:n_points-1), param_m_ncdm(0:n_points-1),&
          param_MG_beta2(0:n_points-1), stat = status1)

     If (status1 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR GRID OF MODELS IN FISHER ANALYSIS ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR GRID OF MODELS IN FISHER ANALYSIS WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     call fill_parameters_array(p) ! FILL ARRAYS OF COSMOLOGICAL PARAMETERS AND WRITE INI FILES (IF NEEDED)

     If (compute_data_fisher_analysis) then

        write(job_number,*) 'SUBMITTING JOBS TO COMPUTE DATA NEEDED FOR FISHER MATRIX ANALYSIS '

        call compute_data_for_fisher_analysis(p)

        stop

     Else

        write(job_number,*) 'USING EXISTING DATA. IF NEW DATA WANTED, CLEAN UP "DATA" FOLDER'

     End If

     ! ALLOCATING MEMORY FOR EL, FIDUCIAL CL (LENSING), FIDUCIAL CL (NOT LENSING), OBSERVED CL, SHOT NOISE, SYSTEMATIC CL
     allocate (El(lmin:lmax,0:nbins,0:nbins),Cl_fid(lmin:lmax,0:nbins,0:nbins),Cl_fid_nl(lmin:lmax,0:nbins,0:nbins),&
          Cl_obs(lmin:lmax,0:nbins,0:nbins), Nl(1:nbins,1:nbins),Cl_syst(lmin:lmax,0:nbins,0:nbins),stat = status3)

     If (status3 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR EL, CL_FID, CL_FID_NL, CL_OBS, NL, CL_SYST ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR EL, CL_FID, CL_FID_NL, CL_OBS, NL, CL_SYST WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     write(job_number,*) 'READING DATA CL_FID, EL, AND CL_FID_NL IN FORTRAN ARRAYS '

     call read_data(Cl_fid,10,filetype,ElCl,.true.,.true.,.true.)

     call read_data(El,10,filetype,ElCl,.true.,.true.,.false.)

     call read_data(Cl_fid_nl,10,filetype,ElCl,.false.,.true.,.true.)

     write(job_number,*) ' COMPUTING AND WRITING OUT SYSTEMATIC ERROR '

     call compute_systematic_error()
 
     call write_Cl_syst(Cl_syst,11)
        
     write(job_number,*) 'COMPUTING SHOT NOISE '

     call compute_shot_noise()

     write(job_number,*) 'COMPUTING OBSERVED CL'

     call compute_observed_Cl()

     write(job_number,*) '-ln(L/L_{max}) AT THE FIDUCIAL POINT (NOT INCLUDING LENSING) IS : ',&
          -euclid_galaxy_cl_likelihood(Cl_fid_nl)

     write(job_number,*), '-ln(L/L_{max}) AT THE FIDUCIAL POINT (INCLUDING LENSING) IS : ', &
          -euclid_galaxy_cl_likelihood(Cl_fid)

     ! ALLOCATING MEMORY FOR COVARIANCE MATRIX
     allocate (cov(lmin:lmax,1:nbins,1:nbins,1:nbins,1:nbins),stat = status4)

     If (status4 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR COV ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR COV WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     write(job_number,*) 'COMPUTING COVARIANCE MATRIX'

     call compute_covariance_matrix()
        
     ! ALLOCATING MEMORY FOR COVARIANCE MATRICES AND THEIR INVERSES 
     allocate (cov_l_IP(lmin:lmax,1:nbins*(nbins+1)/2,1:nbins*(nbins+1)/2),&
          inv_cov_l_IP(lmin:lmax,1:nbins*(nbins+1)/2,1:nbins*(nbins+1)/2),&
          cov_l_IP_oa(lmin:lmax,1:nbins,1:nbins),inv_cov_l_IP_oa(lmin:lmax,1:nbins,1:nbins),stat = status6)

     If (status6 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR COV_L_IP, INV_COV_L_IP, COV_L_IP_OA, INV_COV_L_IP_OA ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR COV_L_IP, INV_COV_L_IP, COV_L_IP_OA, INV_COV_L_IP_OA WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     write(job_number,*) 'COMPUTING COVARIANCE MATRIX IN TERMS OF SUPERINDICES'

     call write_cov_two_indices()

     call write_cov_two_indices_oa()
        
     ! DEALLOCATING MEMORY FOR COVARIANCE MATRIX 
     deallocate (cov)

     write(job_number,*) 'COMPUTING INVERSE OF COVARIANCE MATRICES'

     call inverting_matrix()

     call inverting_matrix_oa()

     ! DEALLOCATING MEMORY FOR COVARIANCE MATRICES
     deallocate (cov_l_IP,cov_l_IP_oa)

     ! ALLOCATING MEMORY FOR INVERSES OF COVARIANCE MATRICES
     allocate (inv_cov(lmin:lmax,1:nbins,1:nbins,1:nbins,1:nbins),inv_cov_oa(lmin:lmax,1:nbins,1:nbins,1:nbins,1:nbins))

     write(job_number,*) 'WRITING INVERSE OF COVARIANCE MATRICES IN TERMS OF INDICES I, J, P, AND Q'

     call write_inv_cov_four_indices()

     call write_inv_cov_four_indices_oa() 
       
     ! DEALLOCATING MEMORY OF INVERSES OF COVARIANCE MATRICES
     deallocate (inv_cov_l_IP,inv_cov_l_IP_oa)

     ! ALLOCATING MEMORY FOR DERIVATIVES OF CL'S
     allocate (Cl_1(lmin:lmax,0:nbins,0:nbins),Cl_2(lmin:lmax,0:nbins,0:nbins),Cl_3(lmin:lmax,0:nbins,0:nbins),&
          Cl_4(lmin:lmax,0:nbins,0:nbins),dCl(lmin:lmax,0:nbins,0:nbins),Cl_5(lmin:lmax,0:nbins,0:nbins),&
          Cl_6(lmin:lmax,0:nbins,0:nbins),Cl_7(lmin:lmax,0:nbins,0:nbins),Cl_8(lmin:lmax,0:nbins,0:nbins),&
          dCl_nl(lmin:lmax,0:nbins,0:nbins),stat = status5)

     If (status5 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR DERIVATIVES OF CL ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR DERIVATIVES OF CL WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     write(job_number,*) 'COMPUTING DERIVATIVES OF CL'

     call compute_derivatives()

     ! DEALLOCATING MEMORY FOR DERIVATIVES OF CL'S
     deallocate (Cl_1,Cl_2,Cl_3,Cl_4,Cl_5,Cl_6,Cl_7,Cl_8,dCl,dCl_nl)

     ! DEALLOCATING MEMORY FOR GRID OF MODELS 
     deallocate (param_omega_b, param_omega_cdm, param_n_s, param_A_s, param_H0, param_m_ncdm,param_MG_beta2)

     ! ALLOCATING MEMORY FOR DERIVATIVES, FISHER MATRIX (INCLUDING LENSING), FISHER MATRIX (NOT INCLUDING LENSING),
     ! INVERSE OF FISHER MATRIX (INCLUDING LENSING), INVERSE OF FISHER MATRIX (NOT INCLUDING LENSING), B_/beta VECTOR,
     ! b_/lambda
     allocate (d1(lmin:lmax,0:nbins,0:nbins),d2(lmin:lmax,0:nbins,0:nbins),d3(lmin:lmax,0:nbins,0:nbins),&
          d4(lmin:lmax,0:nbins,0:nbins),d5(lmin:lmax,0:nbins,0:nbins),d6(lmin:lmax,0:nbins,0:nbins),&
          F_ab(1:number_of_parameters,1:number_of_parameters),F_ab_nl(1:number_of_parameters,1:number_of_parameters),&
          inv_F_ab(1:number_of_parameters,1:number_of_parameters),B_beta(1:number_of_parameters),&
          b_lambda(1:number_of_parameters),d7(lmin:lmax,0:nbins,0:nbins),stat = status1)

     If (status1 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR FISHER MATRIX COMPUTATION ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR FISHER MATRIX COMPUTATION WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     write(job_number,*) 'COMPUTING FISHER MATRIX AND ITS INVERSE (INCLUDING AND NOT INCLUDING LENSING)'

     call compute_fisher_matrix(.true.,.false.) ! FIRST LOGICAL VARIABLE : SET IT TRUE IF INCLUDING LENSING
                                                   ! SECOND LOGICAL VARIABLE : SET IT TRUE IF INCLUDING ONLY AUTO-CORRELATIONS
     call compute_inverse_fisher_matrix()

     call compute_fisher_matrix(.false.,.false.)

     call compute_fisher_matrix(.true.,.true.)

     call compute_fisher_matrix(.false.,.true.)

     call compute_B_beta()

     call compute_b_lambda_alpha()

     ! ALLOCATING MEMORY FOR CL_CURRENT
     allocate (Cl_current(lmin:lmax,0:nbins,0:nbins),stat = status3)

     If (compute_likelihood_along_bias_vector) then

        write(job_number,*) 'COMPUTING RATIO OF LIKELIHOOD ALONG BIAS VECTOR'

        call compute_ratio_likelihood()

     Else 

        continue

     End If

     ! DEALLOCATING MEMORY FOR CL_FID, CL_FID_NL,CL_CURRENT
     deallocate(Cl_fid,Cl_fid_nl,Cl_current)
     ! DEALLOCATING MEMORY FOR DERIVATIVES, FISHER MATRICES, ITS INVERSES, AND BIAS VECTOR
     deallocate (d1,d2,d3,d4,d5,d6,d7,F_ab,F_ab_nl,inv_cov,inv_cov_oa,Cl_syst,B_beta,b_lambda)



     If (do_mcmc_analysis) then

        write(job_number,*) 'FISHER MATRIX ANALYSIS ENDED'

        continue 

     Else

        write(job_number,*) 'FISHER MATRIX ANALYSIS ENDED'

        stop
        
     End If

  Else 

     write(job_number,*) 'NOT DOING FISHER MATRIX ANALYSIS'

     If (do_mcmc_analysis) then

        allocate (El(lmin:lmax,0:nbins,0:nbins),Cl_fid(lmin:lmax,0:nbins,0:nbins),Cl_fid_nl(lmin:lmax,0:nbins,0:nbins),&
          Cl_obs(lmin:lmax,0:nbins,0:nbins), Nl(1:nbins,1:nbins),stat = status3)

        call read_data(Cl_fid,10,filetype,ElCl,.true.,.true.,.true.)

        call read_data(El,10,filetype,ElCl,.true.,.true.,.false.)

        call read_data(Cl_fid_nl,10,filetype,ElCl,.false.,.true.,.true.)

        write(job_number,*) 'COMPUTING SHOT NOISE '

        call compute_shot_noise()

        write(job_number,*) 'COMPUTING OBSERVED CL'

        call compute_observed_Cl()

        call read_data(El,10,filetype,ElCl,lensing,.true.,.false.)

     Else

        stop
                   
     End If

  End If

  !##################################
  ! MARKOV CHAIN MONTE CARLO ANALYSIS
  !##################################

  If (do_mcmc_analysis) then

     write(job_number,*) 'STARTING MCMC ANALYSIS'

     !ALLOCATING MEMORY FOR OLD AND CURRENT POINTS IN PARAMETER SPACE, ACCEPTANCE PROBABILITY, AND CURRENT SPECTRA
     allocate (old_point(1:number_of_parameters),current_point(1:number_of_parameters),&
          acceptance_probability(number_iterations),Cl_current(lmin:lmax,0:nbins,0:nbins),stat = status1)

     If (status1 .eq. 0) then 

        write(job_number,*) 'MEMORY FOR OLD AND CURRENT POINTS IN PARAMETER SPACE, ACCEPTANCE PROBABILITY, AND '
        write(job_number,*) 'CURRENT SPECTRA ALLOCATED SUCCESSFULLY'

     Else 

        write(job_number,*) 'MEMORY FOR OLD_POINT, CURRENT_POINT, ACCEPTANCE_PROBABILITY, AND CL_CURRENT WAS NOT ALLOCATED PROPERLY'

        stop

     End If

     ! ###################################################################################################
     ! GENERATE A RANDOM POINT IN SAME PARAMETER SPACE AS IN FISHER MATRIX ANALYSIS EXCEPT FOR A_S BECAUSE 
     ! THE VERSION OF CLASS DOES NOT TAKE LN()10^10 A_S) AS AN INPUT. RANDOM NUMBER GENERATOR WORKS WITH 
     ! SINGLE PRECISION WHEREAS OUR FUNCTION USE DOUBLE PRECISION; CHANGES ARE MADE ACCORDINGLY.
     ! ###################################################################################################

     If (testing_Gaussian_likelihood) then

        write(job_number,*) 'TESTING MCMC ANALYSIS WITH GAUSSIAN LIKELIHOOD'

        open(17,file='./output/chains/mcmc_final_output.ranges')    ! FILE WITH HARD BOUNDS NEEDED BY GETDIST 

        write(17,*) 'omega_b    N    N '

        write(17,*) 'omega_cdm    N    N '

        write(17,*) 'n_s    N    N '

        write(17,*) 'A_s    N    N '

        write(17,*) 'H0   N    N '

        write(17,*) 'm_ncdm    N    N '

        write(17,*) 'MG_beta2    N    N ' 

        close(17)

        Do i=1,number_of_parameters

           x_old(i) = genunf(-1.,1.)

           old_point(i) = dble(x_old(i))

        End Do

        old_loglikelihood = log_Gaussian_likelihood(old_point)

     Else

        open(16,file='./output/chains/mcmc_final_output.paramnames')    !    FILE WITH NAMES OF PARAMETERS NEEDED BY GETDIST 

        Do i=1,number_of_parameters

           write(16,*) ''//trim(paramnames(i))//'    '//trim(latexname(i))//''

        End Do

        close(16)

        open(17,file='./output/chains/mcmc_final_output.ranges')    !    FILE WITH HARD BOUNDS NEEDED BY GETDIST 

        write(17,*) ''//trim(paramnames(1))//'    6.e-3    3.9e-2 '

        write(17,*) ''//trim(paramnames(2))//'    1.e-2    1. '

        write(17,*) ''//trim(paramnames(3))//'    9.e-2    2. '

        write(17,*) ''//trim(paramnames(4))//'    1.e-9    3.e-9 '

        write(17,*) ''//trim(paramnames(5))//'    50    90 '

        write(17,*) ''//trim(paramnames(6))//'    1.e-4    6.e-1 '

        write(17,*) ''//trim(paramnames(7))//'    1.e-5    2.'

        close(17)

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

        Else If (start_from_bestfit) then 

           call read_bestfit_mcmc(bestfit)

           Do m=1,number_of_parameters

              old_point(m) = bestfit(m)

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

              If (m .eq. 4) then

                 old_point(m) = exp(dble(x_old(m)))/(1.d1**1.d1)

              Else
                 
                 old_point(m) = dble(x_old(m))
                
              End If
            
           End Do

        End If

        !#################################################################
        ! WRITE INI FILE CORRESPONDING TO CURRENT POINT IN PARAMETER SPACE 
        !#################################################################

        inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist)
 
        inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

        If (cl_file_exist) then

           call system('rm ./output/current_euclid_galaxy_cl.dat')

        End If

        If (ini_file_exist) then

           call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')

        End If

        call write_ini_file(old_point(1),old_point(2),old_point(3),old_point(4),old_point(5),old_point(6),&
             old_point(7),tau,N_ur,N_ncdm,deg_ncdm,lensing,selection_sampling_bessel_fid,&
             q_linstep_fid,k_max_tau0_over_l_max_fid)

        !###############################################
        ! RUN CLASS FOR CURRENT POINT IN PARAMETER SPACE
        !###############################################

        inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist) 

        If (ini_file_exist) then

           call run_current_model(lensing) ! REMEMBER THAT LENSING FLAG ALLOWS TO RUN W/O LENSING
 
           inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

           If (cl_file_exist) then

              call read_Cl(Cl_current,11,lensing)

              old_loglikelihood = euclid_galaxy_cl_likelihood(Cl_current)

              call system('rm ./output/current_euclid_galaxy_cl.dat')    ! REMOVE CL FILE 

           Else

              write(job_number,*) 'SOMETHING WENT WRONG WITH CLASS FOR CURRENT MODEL. LOW LIKELIHOOD ASSIGNED FOR CURRENT POINT'

              old_loglikelihood = -1.d10

           End If

           call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')    ! REMOVE INI FILE

        Else

           write(job_number,*) 'NOT INI FILE FOR CURRENT MODEL. SOMETHING WENT WRONG WITH SUBROUTINE "write_ini_file" '

           write(job_number,*) 'LOW LIKELIHOOD ASSIGNED TO CURRENT POINT'
    
           old_loglikelihood = -1.d10

        End if

     End If

     If (multiple_chains) then

        open(13,file=PATH_TO_CHAINS_CHAIN//trim(string)//'.txt') ! FILE TO STORE MCMC COMPUTATION

     Else

        open(13,file=PATH_TO_CHAINS) ! FILE TO STORE MCMC COMPUTATION

     End If

     open(14,file='./output/mcmc_output.txt')    !    TEMPORARY FILE 

     write(13,*) '# NUMBER OF ITERATIONS IN FINAL MCMC : ', number_iterations - steps_taken_before_definite_run

     If (start_from_fiducial .and. (.not.testing_Gaussian_likelihood)) then

        write(job_number,*) '# FIDUCIAL MODEL IS (PARAMETERS ORDERED AS IN WRITTEN IN HEADER OF CHAIN FILE) :', old_point

        write(job_number,*) '# ln(L/L_max) AT THE FIDUCIAL MODEL :', old_loglikelihood

     End If

     write(13,*) '# WEIGHT   -ln(L/L_{max})    ', paramnames(1:number_of_parameters) 

     !#######################################
     ! LOOP TO EXPLORE PARAMETER SPACE STARTS
     !#######################################

     Do m=1,number_iterations

        !######################################################################################################
        ! Generate new point in parameter space from a multivariate normal distribution and the covariance 
        ! matrix computed out of the Fisher matrix  analysis above. We use RANLIB library. Be careful with 
        ! x_old and old_point definitions 
        !######################################################################################################

        Do q=1,number_iterations 

           If (testing_Gaussian_likelihood) then

              call setgmn(x_old,real(Covgauss),number_of_parameters,parm) 
 
              call genmn(parm,x_new,work)

              exit

           Else

              If (using_inverse_fisher_matrix) then

                 If (do_fisher_analysis) then

                    call setgmn(x_old,real(inv_F_ab),number_of_parameters,parm) 

                 Else
                 
                    call setgmn(x_old,real(Covguess),number_of_parameters,parm) 

                 End If

                 call genmn(parm,x_new,work)

              Else

                 call setgmn(x_old,real(Covguess),number_of_parameters,parm) 

                 call genmn(parm,x_new,work)

              End If

           End If

           plausibility(1) = (x_new(1) .lt. real(6.d-3)) .or. (x_new(1) .gt. real(3.9d-2))

           plausibility(2) = (x_new(2) .lt. real(1.d-2)) .or. (x_new(2) .gt. real(1.d0))
           
           plausibility(3) = (x_new(3) .lt. real(9.d-2)) .or. (x_new(3) .gt. real(2.d0))

           plausibility(4) = (x_new(4) .lt. real(log(10.d0))) .or. (x_new(4) .gt. real(log(30.d0))) ! limit As<3.d-9 but using log(10^10As)

           plausibility(5) = (x_new(5) .lt. real(50.d0)).or.(x_new(5).gt.real(90.d0))

           plausibility(6) = (x_new(6) .lt. real(1.d-4)) .or. (x_new(6) .gt. real(6.d-1))

           plausibility(7) = (x_new(7) .le. real(1.d-5)) .or. (x_new(7) .gt. real(2.d0))

           Do n=1,number_of_parameters

              If (plausibility(n)) then

                 x_new(n) = x_old(n)

                 non_plausible_parameters = .false.

              End If

           End Do

           If (non_plausible_parameters .and. (q .ne. number_iterations)) then

              call genmn(parm,x_new,work)

           Else if (q .eq. number_iterations) then

              write(job_number,*) 'LOOP TO GENERATE MULTIVARIATE GAUSSIAN DEVIATE HIT MAXIMUM NUMBER OF ITERATIONS'

              stop

           Else 

              exit

           End If

        End Do
    
        Do n=1,number_of_parameters

           If (n .eq. 4) then

              If (testing_Gaussian_likelihood) then

                 current_point(n) = dble(x_new(n)) ! used if testing Gaussian likelihood

              Else

                 current_point(n) = dexp(dble(x_new(n)))/(1.d1**1.d1) ! Converting log(10**10A_s) to A_s 

              End If

           Else

              current_point(n) = dble(x_new(n))

           End If

        End Do

        If (testing_Gaussian_likelihood) then

           current_loglikelihood = log_Gaussian_likelihood(current_point) 

        Else

           call write_ini_file(current_point(1),current_point(2),current_point(3),current_point(4),&
                current_point(5),current_point(6),current_point(7),tau,N_ur,N_ncdm,deg_ncdm,lensing,&
                selection_sampling_bessel_fid,q_linstep_fid,k_max_tau0_over_l_max_fid)

           !################################
           ! CALL CLASS FOR CURRENT INI FILE
           !################################

           inquire(file='./ini_files/current_euclid_galaxy_cl_.ini',exist=ini_file_exist)
             
           If (ini_file_exist) then

              call run_current_model(lensing) 

              inquire(file='./output/current_euclid_galaxy_cl.dat',exist=cl_file_exist)

              If (cl_file_exist) then

                 call read_Cl(Cl_current,11,lensing)

                 current_loglikelihood = euclid_galaxy_cl_likelihood(Cl_current)

                 call system('rm ./output/current_euclid_galaxy_cl.dat')    ! Remove Cl file

              Else

                 write(job_number,*) 'Cl FILE WAS NOT CREATED, SOMETHING WENT WRONG WITH CLASS FOR THE CURRENT POINT'
                 write(job_number,*) 'LOW LOG LIKELIHOOD ASSIGNED'

                 current_loglikelihood = -1.d10

              End if

              call system('rm ./ini_files/current_euclid_galaxy_cl_.ini')    ! REMOVE INI FILE

           Else

              write(job_number,*) 'NOT INI FILE FOR CURRENT POINT. SOMETHING WENT WRONG WIT SUBROUTINE "write_ini_file" '

              write(job_number,*) 'LOW LOG LIKELIHOOD ASSIGNED '
    
              current_loglikelihood = -1.d10

           End If

        End If

        !###########################
        ! DECIDE ABOUT CURRENT POINT
        !###########################

        If (current_loglikelihood .ge. old_loglikelihood) then ! ACCEPTS CURRENT POINT 

           number_accepted_points = number_accepted_points + 1    

           acceptance_probability(m) = min(1.d0,exp(current_loglikelihood - old_loglikelihood))    

           If (m .le. steps_taken_before_definite_run) then

              write(14,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

           Else

              write(13,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

           End If

           weight = 1    

           old_loglikelihood = current_loglikelihood
        
           Do i=1,number_of_parameters 

              old_point(i) = current_point(i)

              If (i .eq. 4) then

                 If (testing_Gaussian_likelihood) then

                    x_old(i) = real(old_point(i)) 

                 Else

                    x_old(i) = real(log(1.d1**1.d1*old_point(i))) ! converting A_s to log(10**10*A_s)

                 End If

              Else

                 x_old(i) = real(old_point(i))

              End If

           End Do
   
        Else 

           random_uniform = dble(genunf(real(0.),real(1.)))
 
           If ( random_uniform .le. exp(current_loglikelihood-old_loglikelihood)) then ! ACCEPT CURRENT POINT

              number_accepted_points = number_accepted_points + 1 

              acceptance_probability(m) = min(1.d0,exp(current_loglikelihood - old_loglikelihood))    

              If (m .le. steps_taken_before_definite_run) then
                   
                 write(14,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

              else

                 write(13,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

              End If

              weight = 1

              old_loglikelihood = current_loglikelihood

              Do i=1,number_of_parameters 

                 old_point(i) = current_point(i)

                 If (i .eq. 4) then

                    If (testing_Gaussian_likelihood) then

                       x_old(i) = real(old_point(i)) 

                    Else

                       x_old(i) = real(log(1.d1**1.d1*old_point(i))) ! converting A_s to log(10**10*A_s)

                    End If

                 Else

                    x_old(i) = real(old_point(i))

                 End If

              End Do

           Else   ! REJECT CURRENT POINT 

              If (m .gt. steps_taken_before_definite_run) then

                 number_rejected_points = number_rejected_points + 1            

              End If

              acceptance_probability(m) = min(1.d0,exp(current_loglikelihood - old_loglikelihood))    

              weight = weight + 1

              Do i=1,number_of_parameters 

                 If (i .eq. 4) then

                    If (testing_Gaussian_likelihood) then

                       x_old(i) = real(old_point(i))

                    Else

                       x_old(i) = real(log(1.d1**1.d1*old_point(i))) ! convert A_s to log(10**10*A_s)

                    End If

                 Else

                    x_old(i) = real(old_point(i))

                 End If

              End Do

           End If

        End If

        !###################################################################################################
        ! COMPUTE AVERAGE ACCEPTANCE PROBABILITY AND UPDATE COVARIANCE MATRIX AND JUMPING FACTOR (IF NEEDED)
        !###################################################################################################

        If ((mod(m,jumping_factor_update) .eq. 0) .and. (m .le. steps_taken_before_definite_run) ) then

           average_acceptance_probability = sum(acceptance_probability(m-jumping_factor_update+1:m))&
                /real(jumping_factor_update)

           write(job_number,*) 'CURRENT AVERAGE ACCEPTANCE PROBABILITY IS: ',average_acceptance_probability
        
           If (average_acceptance_probability .lt. 0.1) then ! DECREASE STEP SIZE

              jumping_factor = (1.d0 - step_size_changes)  

              If (testing_Gaussian_likelihood) then

                 Covgauss = jumping_factor*Covgauss

              Else

                 Covguess = jumping_factor*Covguess

              End If

           Else if (average_acceptance_probability .gt. 0.4) then ! INCREASE STEP SIZE 

              jumping_factor = (1.d0 + step_size_changes)     

              If (testing_Gaussian_likelihood) then

                 Covgauss = jumping_factor*Covgauss

              Else

                 Covguess = jumping_factor*Covguess

              End If

           End If

           not_good_app = (average_acceptance_probability .lt. 0.1) .or. (average_acceptance_probability .gt. 0.4)
             
           If ( (mod(m,covariance_matrix_update) .eq. 0) .and. not_good_app ) then

              call stat('./output/mcmc_output.txt',buff,status1)

              If ((status1 .eq. 0) .and. (buff(8) .gt. 0)) then

                 If (testing_Gaussian_likelihood) then

                    call system('cd output; python compute_covariance_matrix_Gaussian.py')
     
                    call read_covariance_matrix_mcmc(Covgauss)

                    close(14)
                   
                    call system('rm ./output/mcmc_output.txt')

                    open(14,file='./output/mcmc_output.txt')

                 Else

                    call system('cd output; python compute_covariance_matrix.py')
     
                    call read_covariance_matrix_mcmc(Covguess)

                    close(14)

                    call system('rm ./output/mcmc_output.txt')

                    open(14,file='./output/mcmc_output.txt')

                 End If

              End If

           End If

        End If ! LOOP TO SAMPLE PARAMETER SPACE ENDS

     End Do

     !############################################
     ! WRITE LAST INFORMATIONS AND CLOSE DATA FILE
     !############################################

     write(job_number,*) 'NUMBER OF REJECTED POINTS IS : ', number_rejected_points

     write(job_number,*) 'ACCEPTANCE RATIO IS: ', dble(number_iterations - steps_taken_before_definite_run - &
          number_rejected_points)/dble(number_iterations - steps_taken_before_definite_run)

     close(13)

     close(14)

     If (multiple_chains) then

        continue

     Else

        If (adjusting_covariance_matrix) then

           call system('cd output; python compute_covariance_matrix.py')

           call system('cd analyzer; python analyze_adjusting.py')

        Else

           !call system('cd output; python compute_covariance_matrix_final.py')

           call system('cd analyzer; python analyze.py')

        End If

        call read_bestfit_mcmc(bestfit)

        call read_means_mcmc(means)

        write(job_number,*) 'BESTFIT IS : '
          
        Do m=1,number_of_parameters

           write(job_number,*) ''//trim(paramnames(m))//' = ', bestfit(m)

        End Do

        write(job_number,*) 'MEANS FOR THE SAMPLES ARE : '

        Do m=1,number_of_parameters

           write(job_number,*) ''//trim(paramnames(m))//' = ', means(m)

        End Do

     End If

  Else

     write(job_number,*) 'NOT DOING MCMC ANALYSIS'

  End If

  If (.not. testing_Gaussian_likelihood) then

     deallocate (old_point,current_point,inv_F_ab,Cl_current,Nl,El,Cl_obs,acceptance_probability)

  End If

End Program fisher





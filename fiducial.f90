Module fiducial

  Implicit none

  save 

  !#############################
  ! PARAMETERS OF FIDUCIAL MODEL
  !#############################
  
  Real*8,parameter :: omega_b = 2.218d-2
  Real*8,parameter :: omega_cdm = 1.205d-1
  Real*8,parameter :: n_s = 9.619d-1
  Real*8,parameter :: A_s = 2.12424d-9
  Real*8,parameter :: H0 = 6.693d1
  Real*8,parameter :: m_ncdm = 6.0d-2
  Real*8,parameter :: N_ur = 2.0328d0
  Real*8,parameter :: N_ncdm = 1.d0
  Real*8,parameter :: deg_ncdm = 1.d0
  Real*8,parameter :: tau = 5.96d-2
  Real*8,parameter :: nc_bias_b0 = 1.0d0
  Real*8,parameter :: cs2_fld = 1.d-4 ! 1.d0 ; 1.d-4 ; 1.d-6 ; 3.333334d0 ; 3.3334d0 ; 4.3d0 
  Real*8,parameter :: w0_fld = -8.0d-1 
  !Real*8,parameter :: wa_fld = 0.d0 
  Real*8,parameter :: e_pi = 0.0d0
  Real*8,parameter :: f_pi = 0.0d0 ! 0.d0 ; 5.d0
  Real*8,parameter :: g_pi = 0.0d0 ! 0.d0 ; 1.d0 ! THIS IS ACTUALLY log10 g_pi


  Character(len=*),parameter :: param_name_omega_b = 'omega_b'
  Character(len=*),parameter :: param_name_omega_cdm = 'omega_cdm'
  Character(len=*),parameter :: param_name_n_s = 'n_s'
  Character(len=*),parameter :: param_name_A_s = 'A_s'
  Character(len=*),parameter :: param_name_H0 = 'H0'
  Character(len=*),parameter :: param_name_m_ncdm = 'm_ncdm'
  Character(len=*),parameter :: param_name_nc_bias_b0 = 'nc_bias_b0'
  Character(len=*),parameter :: param_name_tau_reio = 'tau_reio'
  Character(len=*),parameter :: param_name_cs2_fld = 'cs2_fld'
  Character(len=*),parameter :: param_name_w0_fld = 'w0_fld'
  !Character(len=*),parameter :: param_name_wa_fld = 'wa_fld'
  Character(len=*),parameter :: param_name_e_pi = 'e_pi'
  Character(len=*),parameter :: param_name_f_pi = 'f_pi'
  Character(len=*),parameter :: param_name_g_pi = 'g_pi'

  !################################################
  ! 1-SIGMA VALUES FOR PARAMETERS IN FIDUCIAL MODEL
  !################################################

  Real*8,parameter :: sigma_omega_b = 1.5d-4
  Real*8,parameter :: sigma_omega_cdm = 1.4d-3
  Real*8,parameter :: sigma_n_s = 4.5d-3
  Real*8,parameter :: sigma_A_s = 3.82d-11
  Real*8,parameter :: sigma_H0 = 0.62d0
  Real*8,parameter :: sigma_m_ncdm = 5.d-3
  Real*8,parameter :: sigma_tau = 8.9d-3
  Real*8,parameter :: sigma_nc_bias_b0 = 1.0d-1
  Real*8,parameter :: sigma_cs2_fld = 1.0d-1
  Real*8,parameter :: sigma_w0_fld = 2.2d-1
  !Real*8,parameter :: sigma_wa_fld = 2.2d-1
  Real*8,parameter :: sigma_e_pi = 1.0d-1
  Real*8,parameter :: sigma_f_pi = 1.0d-1
  Real*8,parameter :: sigma_g_pi = 1.0d0 ! THIS IS THE ERROR ON log10 g_pi 


  !################################
  ! CLASS AND SURVEY SPECIFICATIONS. PLANCK SPECIFICATIONS COME FROM TABLE 1.1 IN 'PLANCK THE SCIENTIFIC PROGRAMME (2005)'
  !################################

  Integer*4,parameter :: nbins = 5
  Integer*4,parameter :: lmax_class = 2000
  Integer*4,parameter :: lmin = 2
  Integer*4,parameter :: lmax_class_cmb = 2500  ! FAKE PLANCK
  Integer*4,parameter :: number_of_channels = 3 ! FAKE PLANCK

  Real*8,parameter    :: Pi = 3.141592653589793d0
  Real*8,parameter    :: fsky_planck = 6.5d-1   ! FAKE PLANCK
  Real*8,dimension(number_of_channels),parameter :: theta_fwhm = [1.d1*Pi/1.08d4,7.1d0*Pi/1.08d4,5.d0*Pi/1.08d4] ! ANGULAR RESOLUTION IN RADIANS
  Real*8,dimension(number_of_channels),parameter :: sigma_T = [6.82d1,4.26d1,6.54d1]
  Real*8,dimension(number_of_channels),parameter :: sigma_P = [1.09d2,8.13d1,1.336d2]
  Real*8,parameter    :: zmin = 0.1d0
  Real*8,parameter    :: zmax = 2.0d0
  Real*8,parameter    :: dz = 1.0d-3
  Real*8,parameter    :: gal_per_sqarcmn = 30.d0
  Real*8,parameter    :: fsky = 1.5d4/4.1253d4
  Real*8,parameter    :: theoreticalerror = 0.d0 !5.d-2
  Real*8,parameter    :: l_switch_limber_for_nc_local_over_z = 20000.d0
  Real*8,parameter    :: l_switch_limber_for_nc_los_over_z = 1000.d0
  Real*8,parameter    :: selection_sampling_bessel_fid = 3.d0 ! FIDUCIAL PRECISION PARAMETER FOR FISHER ANALYSIS
  Real*8,parameter    :: q_linstep_fid = 0.3d0                ! FIDUCIAL PRECISION PARAMETER FOR FISHER ANALYSIS
  Real*8,parameter    :: k_max_tau0_over_l_max_fid = 20.d0    ! FIDUCIAL PRECISION PARAMETER FOR FISHER ANALYSIS
  Real*8,parameter    :: selection_sampling_bessel_mcmc = 1.2d0 ! FIDUCIAL PRECISION PARAMETER FOR MCMC ANALYSIS (WITHOUT LENSING)
  Real*8,parameter    :: q_linstep_mcmc = 40.d0                 ! FIDUCIAL PRECISION PARAMETER FOR MCMC ANALYSIS (WITHOUT LENSING)
  Real*8,parameter    :: k_max_tau0_over_l_max_mcmc = 2.d0      ! FIDUCIAL PRECISION PARAMETER FOR MCMC ANALYSIS (WITHOUT LENSING)

  !##################
  ! FISHER PARAMETERS
  !##################

  Integer*4,parameter :: n_points = 5 ! NUMBER OF POINTS PER COSMOLOGICAL PARAMETER
  Integer*4,parameter :: lmax = 400   ! HIGHEST MULTIPOLE

  Logical,parameter   :: compute_data_fisher_analysis = .false.   ! COMPUTE DATA FOR FISHER ANALYSIS IF SET IT TRUE
  Logical,parameter   :: do_fisher_analysis = .false. ! DO FISHER MATRIX ANALYSIS IF SET IT TRUE
  Logical,parameter   :: fisher_analysis_at_bestfit = .false. !.true.  ! DO FISHER MATRIX ANALYSIS AT THE BEST FIT IF SET IT TRUE, OTHERWISE AT THE FIDUCIAL MODEL
  Logical,parameter   :: testing_precision = .false. ! PERFORM PRECISION TEST IF SET IT TRUE
  Logical,parameter   :: compute_data_testing_precision = .false. ! COMPUTE DATA FOR PRECISION TEST IF SET IT TRUE
  Logical,parameter   :: compute_likelihood_along_bias_vector = .false. 
  
  !################
  ! MCMC PARAMETERS
  !################

  Integer*4,parameter    :: number_iterations = 8000 !11000000        ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
  Integer*4,parameter    :: number_of_parameters = 10       ! NUMBER OF COSMOLOGICAL PARAMETERS: 10 FOR DEA MODEL ONLY INCLUDING e_pi; 11 FOR DEA MODEL INCLUDING 
  ! f_pi and g_pi; 12 FOR DEA MODEL INCLUDING e_pi, f_pi, and g_pi; 11 FOR DEA MODEL ONLY INCLUDING e_pi BUT JOINTLY ANALYSING NUMBER COUNTS AND CMB DATA; 
  ! 12 FOR DEA MODEL INCLUDING  f_pi and g_pi BUT JOINTLY ANALYSING NUMBER COUNTS AND CMB DATA; 13 FOR DEA MODEL INCLUDING e_pi, f_pi, and g_pi BUT JOINTLY ANALYSING  ! NUMBER COUNTS AND CMB DATA   
  Integer*4,parameter    :: DEA_MODEL = 1 ! 1: DEA MODEL ONLY INCLUDING e_pi; 2: DEA MODEL INCLUDING f_pi and g_pi; 3: DEA MODEL INCLUDING e_pi, f_pi, g_pi 
  Integer*4,parameter    :: number_DEA_parameters = DEA_MODEL 
  Integer*4,parameter    :: jumping_factor_update = 100    ! STEPS TAKEN BEFORE UPDATING JUMPING FACTOR (IF NEEDED)
  Integer*4,parameter    :: covariance_matrix_update = 0 !5000!10000 ! STEPS TAKEN BEFORE UPDATING COVARIANCE MATRIX (IF NEEDED)
  Integer*4,parameter    :: steps_taken_before_definite_run = 0 !5000!10000 ! STEPS TAKEN BEFORE FREEZING COVARIANCE MATRIX
  Integer*4,parameter    :: number_of_parallel_jobs = 9 ! NUMBER OF JOBS FOR DEFINITE MCMC RUN
  Integer*4,parameter    :: UNIT_RANGES_FILE = 90        ! UNIT NUMBER FOR RANGES FILE
  Integer*4,parameter    :: UNIT_PARAMNAMES_FILE = 91    ! UNIT NUMBER FOR PARAMMNAMES FILE
  Integer*4,parameter    :: UNIT_MCMC = 92               ! UNIT NUMBER FOR MCMC OUTPUT (CALIBRATING PHASE)
  Integer*4,parameter    :: UNIT_MCMC_FINAL = 93         ! UNIT NUMBER FOR MCMC FINAL OUTPUT 

  Real*8,parameter       :: step_size_changes = 1.d-2      ! CHANGE IN STEP SIZE

  Character*16,parameter :: phrase = 'randomizer'       ! PHRASE NEEDED BY NUMBER RANDOM GENERATOR
  ! MODEL 1
  Character(len=10),dimension(number_of_parameters), parameter :: paramnames = ['omega_b   ','omega_cdm ','   n_s    ',&
       '   A_s    ','   H0     ','  m_ncdm  ','nc_bias_b0','cs2_fld   ','w0_fld    ','   e_pi   ']!,'   f_pi   ','   g_pi   ']
  ! MODEL 1 NUMBER COUNTS + CMB 
!  Character(len=10),dimension(number_of_parameters), parameter :: paramnames = ['omega_b   ','omega_cdm ','   n_s    ',&
!       '   A_s    ','   H0     ','  m_ncdm  ','nc_bias_b0','cs2_fld   ','w0_fld    ','   e_pi   ','   tau    ']
  ! MODEL 2 NUMBER COUNTS ALONE
!  Character(len=10),dimension(number_of_parameters), parameter :: paramnames = ['omega_b   ','omega_cdm ','   n_s    ',&
!       '   A_s    ','   H0     ','  m_ncdm  ','nc_bias_b0','cs2_fld   ','w0_fld    ','   f_pi   ','   g_pi   ']
  ! MODEL 2 NUMBER COUNTS + CMB
!  Character(len=10),dimension(number_of_parameters), parameter :: paramnames = ['omega_b   ','omega_cdm ','   n_s    ',&
!       '   A_s    ','   H0     ','  m_ncdm  ','nc_bias_b0','cs2_fld   ','w0_fld    ','   f_pi   ','   g_pi   ','   tau    ']
  ! MODEL 3 NUMBER COUNTS ALONE
!  Character(len=10),dimension(number_of_parameters), parameter :: paramnames = ['omega_b   ','omega_cdm ','   n_s    ',&
!       '   A_s    ','   H0     ','  m_ncdm  ','nc_bias_b0','cs2_fld   ','w0_fld    ','   e_pi   ','   f_pi   ','   g_pi   ']
  ! MODEL 3 NUMBER COUNTS + CMB
!  Character(len=10),dimension(number_of_parameters), parameter :: paramnames = ['omega_b   ','omega_cdm ','   n_s    ',&
!       '   A_s    ','   H0     ','  m_ncdm  ','nc_bias_b0','cs2_fld   ','w0_fld    ','   e_pi   ','   f_pi   ','   g_pi   ',&
!'   tau    ']
  ! MODEL 1 NUMBER COUNTS ALONE
  Character(len=12),dimension(number_of_parameters), parameter :: latexname = ['\omega_b    ','\omega_{cdm}','n_s         ',&
       'A_s         ','H_0         ','m_{\nu}     ','b_0         ','c_s^2       ','w_0         ','e_{\pi}     ']
  ! MODEL 1 NUMBER COUNTS + CMB
!  Character(len=12),dimension(number_of_parameters), parameter :: latexname = ['\omega_b    ','\omega_{cdm}','n_s         ',&
!       'A_s         ','H_0         ','m_{\nu}     ','b_0         ','c_s^2       ','w_0         ','e_{\pi}     ','\tau        ']
  ! MODEL 2 NUMBER COUNTS ALONE
!  Character(len=12),dimension(number_of_parameters), parameter :: latexname = ['\omega_b    ','\omega_{cdm}','n_s         ',&
!       'A_s         ','H_0         ','m_{\nu}     ','b_0         ','c_s^2       ','w_0         ','f_{\pi}     ','g_{\pi}     ']
  ! MODEL 2 NUMBER COUNTS + CMB
!  Character(len=12),dimension(number_of_parameters), parameter :: latexname = ['\omega_b    ','\omega_{cdm}','n_s         ',&
!       'A_s         ','H_0         ','m_{\nu}     ','b_0         ','c_s^2       ','w_0         ','f_{\pi}     ','g_{\pi}     ',&
  !'\tau        ']
  ! MODEL 3 NUMBER COUNTS ALONE
!  Character(len=12),dimension(number_of_parameters), parameter :: latexname = ['\omega_b    ','\omega_{cdm}','n_s         ',&
!       'A_s         ','H_0         ','m_{\nu}     ','b_0         ','c_s^2       ','w_0         ','e_{\pi}     ','f_{\pi}     ','g_{\pi}     ']
  ! MODEL 3 NUMBER COUNTS + CMB
!  Character(len=12),dimension(number_of_parameters), parameter :: latexname = ['\omega_b    ','\omega_{cdm}','n_s         ',&
!       'A_s         ','H_0         ','m_{\nu}     ','b_0         ','c_s^2       ','w_0         ','e_{\pi}     ','f_{\pi}     ',&
  !'g_{\pi}     ','\tau        ']

  Logical,parameter      :: using_inverse_fisher_matrix = .false. !.true. !  USE INVERSE OF FISHER MATRIX AS A COVARIANCE MATRIX IF SET IT TRUE  
  Logical,parameter      :: do_mcmc_analysis = .true.    ! DO MCMC ANALYSIS IF SET IT TRUE
  Logical,parameter      :: start_from_fiducial = .false.    ! START MCMC ANALYSIS FROM FIDUCIAL POINT IF SET IT TRUE
  Logical,parameter      :: start_from_bestfit = .false.    ! START MCMC ANALYSIS FROM BESTFIT IF SET IT TRUE
  Logical,parameter      :: testing_Gaussian_likelihood = .false.  ! TEST GAUSSIAN LIKELIHOOD IF SET IT TRUE
  Logical,parameter      :: adjusting_covariance_matrix = .false.!.true.  ! UPDATE JUMPING FACTOR AND COVARIANCE MATRIX IF SET IT TRUE
  Logical,parameter      :: read_covariance_matrix_from_file = .false. !.true. ! READ COVARIANCE MATRIX FROM FILE IF SET IT TRUE
  Logical,parameter      :: use_getdist = .false. ! USE GETDIST WHEN RUNNIG THE CODE IF SET IT TRUE
  Logical,parameter      :: multiple_chains = .true. !.true.!.false. ! USED TO RUN SEVERAL CHAINS WITH SAME COVARIANCE MATRIX IF SET IT TRUE
  Logical,parameter      :: use_only_autocorrelations = .false. ! COMPUTE LIKELIHOOD INCLUDING ONLY AUTOCORRELATIONS IF SET IT TRUE
  Logical,parameter      :: use_gaussian_planck_prior = .false. !.true. ! USE GAUSSIAN PRIOR BASED ON PAPER XIII (2015) IF SET IT TRUE
  Logical,parameter      :: include_fake_planck_likelihood = .false. ! INCLUDE FAKE PLANCK LIKELIHOOD IN THE ANALYSIS IF SET IT TRUE

  !###############
  ! PATHS TO FILES
  !###############

  Character(len=*),parameter :: Execution_information = './output/chains/execution_information.txt'
  Character(len=*),parameter :: EXECUTION_INFORMATION_CHAIN = './output/chains/execution_information_chain_'
  Character(len=*),parameter :: PATH_TO_CHAINS = './output/chains/mcmc_final_output.txt'
  Character(len=*),parameter :: PATH_TO_CHAINS_CHAIN = './output/chains/mcmc_final_output_'
  Character(len=*),parameter :: PATH_TO_RANGES_FILE = './output/chains/mcmc_final_output.ranges'
  Character(len=*),parameter :: PATH_TO_PARAMNAMES_FILE = './output/chains/mcmc_final_output.paramnames'
  Character(len=*),parameter :: PATH_TO_INI_FILES = './ini_files/current_euclid_galaxy_cl_'
  Character(len=*),parameter :: PATH_TO_CURRENT_CL = './output/current_euclid_galaxy_cl_'
  Character(len=*),parameter :: PATH_TO_INI_FILES_CMB = './ini_files/current_fake_planck_cl_'
  Character(len=*),parameter :: PATH_TO_CURRENT_CL_CMB = './output/current_fake_planck_'    

End Module fiducial

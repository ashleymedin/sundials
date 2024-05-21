! 
! -----------------------------------------------------------------
! Programmer: Ashley E. Van Beusekom @ USask
! -----------------------------------------------------------------
! This file defines the fortran ExecPolicy classes which
! are utilized to determine CUDA kernel launch parameters.
! -----------------------------------------------------------------
! 

module fsundials_cuda_policies_mod
  use, intrinsic :: ISO_C_BINDING
  implicit none

  integer, parameter :: WARP_SIZE = 32
  type, bind(c) :: cudaStream_t
    type(c_ptr) :: ptr = c_null_ptr
  end type cudaStream_t

  type, public :: SUNCudaExecPolicy
      type(cudaStream_t) :: stream
    contains
      procedure :: gridSize
      procedure :: blockSize
      !procedure :: clone
      procedure :: atomic
  end type SUNCudaExecPolicy

  type, extends(SUNCudaExecPolicy) :: SUNCudaThreadDirectExecPolicy
    integer :: blockDim_
  contains
    procedure :: gridSize => threadDirectGridSize
    procedure :: blockSize => threadDirectBlockSize
    procedure :: clone => threadDirectClone
    procedure :: atomic => atomic_false_1
  end type SUNCudaThreadDirectExecPolicy

  type, public, extends(SUNCudaExecPolicy) :: SUNCudaGridStrideExecPolicy
    integer :: gridDim_, blockDim_
  contains
    procedure :: gridSize => gridStrideGridSize
    procedure :: blockSize => gridStrideBlockSize
    procedure :: clone => gridStrideClone
    procedure :: atomic => atomic_false_2
  end type SUNCudaGridStrideExecPolicy

  type, public, extends(SUNCudaExecPolicy) :: SUNCudaBlockReduceAtomicExecPolicy
    integer :: gridDim_, blockDim_
  contains
    procedure :: gridSize => blockReduceAtomicGridSize
    procedure :: blockSize => blockReduceAtomicBlockSize
    procedure :: clone => blockReduceAtomicClone
    procedure :: atomic => atomic_true
  end type SUNCudaBlockReduceAtomicExecPolicy

  type, public, extends(SUNCudaExecPolicy) :: SUNCudaBlockReduceExecPolicy
    integer :: gridDim_, blockDim_
  contains
    procedure :: gridSize => blockReduceGridSize
    procedure :: blockSize => blockReduceBlockSize
    procedure :: clone => blockReduceClone
    procedure :: atomic => atomic_false
  end type SUNCudaBlockReduceExecPolicy

contains

  integer(c_int) function gridSize(this, numWorkUnits, blockDim) result(gridSize_)
    class(SUNCudaExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    gridSize_ = 0
  end function gridSize

  integer(c_int) function blockSize(this, numWorkUnits, gridDim) result(blockSize_)
    class(SUNCudaExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    blockSize_ = 0
  end function blockSize

  !function clone(this, stream) result(clone_)
  !  class(SUNCudaExecPolicy), intent(in) :: this
  !  type(cudaStream_t), intent(in) :: stream    
  !  type(SUNCudaExecPolicy), pointer :: clone_
  !  allocate(clone_)
  !  clone_ = 0
  !end function clone

  logical function atomic(this) result(atomic_)
    class(SUNCudaExecPolicy), intent(in) :: this
    atomic_ = .false.
  end function atomic

  integer(c_int) function threadDirectGridSize(this, numWorkUnits, blockDim) result(gridSize_)
    class(SUNCudaThreadDirectExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    if (present(numWorkUnits)) then
      gridSize_ = (numWorkUnits + this%blockSize() - 1) / this%blockSize()
    else
      gridSize_ = 0
    end if
  end function threadDirectGridSize

  integer(c_int) function threadDirectBlockSize(this, numWorkUnits, gridDim) result(blockSize_)
    class(SUNCudaThreadDirectExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    blockSize_ = this%blockDim_
  end function threadDirectBlockSize

  function threadDirectClone(this, stream) result(clone_)
    class(SUNCudaThreadDirectExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream    
    type(SUNCudaThreadDirectExecPolicy), pointer :: clone_
    allocate(clone_)
    clone_%blockDim_ = this%blockDim_
    clone_%stream = this%stream
  end function threadDirectClone

  logical function atomic_false_1(this) result(atomic_)
    class(SUNCudaThreadDirectExecPolicy), intent(in) :: this
    atomic_ = .false.
  end function atomic_false_1


  integer(c_int) function gridStrideGridSize(this, numWorkUnits, blockDim) result(gridSize_)
    class(SUNCudaGridStrideExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    gridSize_ = this%gridDim_
  end function gridStrideGridSize

  integer(c_int) function gridStrideBlockSize(this, numWorkUnits, gridDim) result(blockSize_)
    class(SUNCudaGridStrideExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    blockSize_ = this%blockDim_
  end function gridStrideBlockSize
  
  function gridStrideClone(this, stream) result(clone_)
    class(SUNCudaGridStrideExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream
    type(SUNCudaGridStrideExecPolicy), pointer :: clone_
    allocate(clone_)
    clone_%gridDim_ = this%gridDim_
    clone_%blockDim_ = this%blockDim_
    clone_%stream = this%stream
  end function gridStrideClone

  logical function atomic_false_2(this) result(atomic_)
    class(SUNCudaGridStrideExecPolicy), intent(in) :: this
    atomic_ = .false.
  end function atomic_false_2

  
  integer(c_int) function blockReduceAtomicGridSize(this, numWorkUnits, blockDim) result(gridSize_)
    class(SUNCudaBlockReduceAtomicExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    if (blockDim < 1 .or. mod(blockDim, WARP_SIZE) /= 0) then
      print *, "Error: the block size must be a multiple of the CUDA warp size"
      stop 1
    end if
    if (this%gridDim_ == 0) then
      if (present(numWorkUnits)) then
        gridSize_ = (numWorkUnits + (this%blockSize() * 2 - 1)) / (this%blockSize() * 2)
      else
        gridSize_ = 0
      end if
    else
      gridSize_ = this%gridDim_
    end if
  end function blockReduceAtomicGridSize

  integer(c_int) function blockReduceAtomicBlockSize(this, numWorkUnits, gridDim) result(blockSize_)
    class(SUNCudaBlockReduceAtomicExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    blockSize_ = this%blockDim_
  end function blockReduceAtomicBlockSize

  function blockReduceAtomicClone(this, stream) result(clone_)
    class(SUNCudaBlockReduceAtomicExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream
    type(SUNCudaBlockReduceAtomicExecPolicy), pointer :: clone_
    allocate(clone_)
    clone_%gridDim_ = this%gridDim_
    clone_%blockDim_ = this%blockDim_
    clone_%stream = this%stream
  end function blockReduceAtomicClone

  logical function atomic_true(this) result(atomic_)
    class(SUNCudaBlockReduceAtomicExecPolicy), intent(in) :: this
    atomic_ = .true.
  end function atomic_true


  integer(c_int) function blockReduceGridSize(this, numWorkUnits, blockDim) result(gridSize_)
    class(SUNCudaBlockReduceExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    if (blockDim < 1 .or. mod(blockDim, WARP_SIZE) /= 0) then
      print *, "Error: the block size must be a multiple of the CUDA warp size"
      stop 1
    end if
    if (this%gridDim_ == 0) then
      if (present(numWorkUnits)) then
        gridSize_ = (numWorkUnits + (this%blockSize() * 2 - 1)) / (this%blockSize() * 2)
      else
        gridSize_ = 0
      end if
    else
      gridSize_ = this%gridDim_
    end if
  end function blockReduceGridSize

  integer(c_int) function blockReduceBlockSize(this, numWorkUnits, gridDim) result(blockSize_)
    class(SUNCudaBlockReduceExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    blockSize_ = this%blockDim_
  end function blockReduceBlockSize

  function blockReduceClone(this, stream) result(clone_)
    class(SUNCudaBlockReduceExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream
    type(SUNCudaBlockReduceExecPolicy), pointer :: clone_
    allocate(clone_)
    clone_%gridDim_ = this%gridDim_
    clone_%blockDim_ = this%blockDim_
    clone_%stream = this%stream
  end function blockReduceClone

  logical function atomic_false(this) result(atomic_)
    class(SUNCudaBlockReduceExecPolicy), intent(in) :: this
    atomic_ = .false.
  end function atomic_false

end module fsundials_cuda_policies_mod
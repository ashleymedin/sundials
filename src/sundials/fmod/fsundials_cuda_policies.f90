module fsundials_cuda_policies_mod
  use, intrinsic :: ISO_C_BINDING
  implicit none

  integer, parameter :: WARP_SIZE = 32
  type, bind(c) :: cudaStream_t
    type(c_ptr) :: ptr = c_null_ptr
  end type cudaStream_t

  type, abstract :: ExecPolicy
    type(cudaStream_t) :: stream
  contains
    procedure(gridSize_interface), deferred, pass :: gridSize
    procedure(blockSize_interface), deferred, pass :: blockSize
    !procedure(clone_interface), deferred, pass :: clone
    procedure(atomic_interface), deferred, pass :: atomic
  end type ExecPolicy

  type, extends(ExecPolicy) :: ThreadDirectExecPolicy
    integer :: blockDim_
  contains
    procedure :: gridSize => threadDirectGridSize
    procedure :: blockSize => threadDirectBlockSize
    procedure :: clone => threadDirectClone
    procedure :: atomic => atomic_false_1
  end type ThreadDirectExecPolicy

  type, extends(ExecPolicy) :: GridStrideExecPolicy
    integer :: gridDim_, blockDim_
  contains
    procedure :: gridSize => gridStrideGridSize
    procedure :: blockSize => gridStrideBlockSize
    procedure :: clone => gridStrideClone
    procedure :: atomic => atomic_false_2

  end type GridStrideExecPolicy

  type, extends(ExecPolicy) :: BlockReduceAtomicExecPolicy
    integer :: gridDim_, blockDim_
  contains
    procedure :: gridSize => blockReduceAtomicGridSize
    procedure :: blockSize => blockReduceAtomicBlockSize
    procedure :: clone => blockReduceAtomicClone
    procedure :: atomic => atomic_true
  end type BlockReduceAtomicExecPolicy

  type, extends(ExecPolicy) :: BlockReduceExecPolicy
    integer :: gridDim_, blockDim_
  contains
    procedure :: gridSize => blockReduceGridSize
    procedure :: blockSize => blockReduceBlockSize
    procedure :: clone => blockReduceClone
    procedure :: atomic => atomic_false
  end type BlockReduceExecPolicy


  abstract interface
    function gridSize_interface(this, numWorkUnits, blockDim) result(gridSize)
      import :: ExecPolicy, c_int
      class(ExecPolicy), intent(in) :: this
      integer(c_int), intent(in), optional :: numWorkUnits, blockDim
      integer(c_int) :: gridSize
    end function gridSize_interface

    function blockSize_interface(this, numWorkUnits, gridDim) result(blockSize)
      import :: ExecPolicy, c_int
      class(ExecPolicy), intent(in) :: this
      integer(c_int), intent(in), optional :: numWorkUnits, gridDim
      integer(c_int) :: blockSize
    end function blockSize_interface

    !function clone_interface(this) result(clone)
    !  import :: ExecPolicy
    !  class(ExecPolicy), intent(in) :: this
    !  type(ExecPolicy), pointer :: clone
    !end function clone_interface

    function atomic_interface(this) result(atomic)
      import :: ExecPolicy
      class(ExecPolicy), intent(in) :: this
      logical :: atomic
    end function atomic_interface

  end interface

contains

  function threadDirectGridSize(this, numWorkUnits, blockDim) result(gridSize)
    class(ThreadDirectExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    integer :: gridSize
    if (present(numWorkUnits)) then
      gridSize = (numWorkUnits + this%blockSize() - 1) / this%blockSize()
    else
      gridSize = 0
    end if
  end function threadDirectGridSize

  function threadDirectBlockSize(this, numWorkUnits, gridDim) result(blockSize)
    class(ThreadDirectExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    integer :: blockSize
    blockSize = this%blockDim_
  end function threadDirectBlockSize

  function threadDirectClone(this, stream) result(clone)
    class(ThreadDirectExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream    
    type(ThreadDirectExecPolicy), pointer :: clone
    allocate(clone)
    clone%blockDim_ = this%blockDim_
    clone%stream = this%stream
  end function threadDirectClone

  function atomic_false_1(this) result(atomic)
    class(ThreadDirectExecPolicy), intent(in) :: this
    logical :: atomic
    atomic = .false.
  end function atomic_false_1


  function gridStrideGridSize(this, numWorkUnits, blockDim) result(gridSize)
    class(GridStrideExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    integer :: gridSize
    gridSize = this%gridDim_
  end function gridStrideGridSize

  function gridStrideBlockSize(this, numWorkUnits, gridDim) result(blockSize)
    class(GridStrideExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    integer :: blockSize
    blockSize = this%blockDim_
  end function gridStrideBlockSize
  
  function gridStrideClone(this, stream) result(clone)
    class(GridStrideExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream
    type(GridStrideExecPolicy), pointer :: clone
    allocate(clone)
    clone%gridDim_ = this%gridDim_
    clone%blockDim_ = this%blockDim_
    clone%stream = this%stream
  end function gridStrideClone

  function atomic_false_2(this) result(atomic)
    class(GridStrideExecPolicy), intent(in) :: this
    logical :: atomic
    atomic = .false.
  end function atomic_false_2

  
  function blockReduceAtomicGridSize(this, numWorkUnits, blockDim) result(gridSize)
    class(BlockReduceAtomicExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    integer :: gridSize
    if (blockDim < 1 .or. mod(blockDim, WARP_SIZE) /= 0) then
      print *, "Error: the block size must be a multiple of the CUDA warp size"
      stop 1
    end if
    if (this%gridDim_ == 0) then
      if (present(numWorkUnits)) then
        gridSize = (numWorkUnits + (this%blockSize() * 2 - 1)) / (this%blockSize() * 2)
      else
        gridSize = 0
      end if
    else
      gridSize = this%gridDim_
    end if
  end function blockReduceAtomicGridSize

  function blockReduceAtomicBlockSize(this, numWorkUnits, gridDim) result(blockSize)
    class(BlockReduceAtomicExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    integer :: blockSize
    blockSize = this%blockDim_
  end function blockReduceAtomicBlockSize

  function blockReduceAtomicClone(this, stream) result(clone)
    class(BlockReduceAtomicExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream
    type(BlockReduceAtomicExecPolicy), pointer :: clone
    allocate(clone)
    clone%gridDim_ = this%gridDim_
    clone%blockDim_ = this%blockDim_
    clone%stream = this%stream
  end function blockReduceAtomicClone

  function atomic_true(this) result(atomic)
    class(BlockReduceAtomicExecPolicy), intent(in) :: this
    logical :: atomic
    atomic = .true.
  end function atomic_true


  function blockReduceGridSize(this, numWorkUnits, blockDim) result(gridSize)
    class(BlockReduceExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, blockDim
    integer :: gridSize
    if (blockDim < 1 .or. mod(blockDim, WARP_SIZE) /= 0) then
      print *, "Error: the block size must be a multiple of the CUDA warp size"
      stop 1
    end if
    if (this%gridDim_ == 0) then
      if (present(numWorkUnits)) then
        gridSize = (numWorkUnits + (this%blockSize() * 2 - 1)) / (this%blockSize() * 2)
      else
        gridSize = 0
      end if
    else
      gridSize = this%gridDim_
    end if
  end function blockReduceGridSize

  function blockReduceBlockSize(this, numWorkUnits, gridDim) result(blockSize)
    class(BlockReduceExecPolicy), intent(in) :: this
    integer, intent(in), optional :: numWorkUnits, gridDim
    integer :: blockSize
    blockSize = this%blockDim_
  end function blockReduceBlockSize

  function blockReduceClone(this, stream) result(clone)
    class(BlockReduceExecPolicy), intent(in) :: this
    type(cudaStream_t), intent(in) :: stream
    type(BlockReduceExecPolicy), pointer :: clone
    allocate(clone)
    clone%gridDim_ = this%gridDim_
    clone%blockDim_ = this%blockDim_
    clone%stream = this%stream
  end function blockReduceClone

  function atomic_false(this) result(atomic)
    class(BlockReduceExecPolicy), intent(in) :: this
    logical :: atomic
    atomic = .false.
  end function atomic_false

end module fsundials_cuda_policies_mod
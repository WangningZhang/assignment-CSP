program FourierTransform
  implicit none
  
  integer, parameter :: N = 100      ! 时间序列长度
  real, parameter :: PI = 3.1415926  ! 圆周率
  
  integer :: i, j                   ! 循环索引
  real :: dt                        ! 时间步长
  real :: t(N), signal(N)           ! 时间和信号数组
  complex :: freq(N), spectrum(N)   ! 频率和频谱数组
  
  ! 生成时间序列
  dt = 1.0 / N
  do i = 1, N
    t(i) = (i - 1) * dt
    signal(i) = sin(2.0 * PI * 5.0 * t(i)) + 0.5 * sin(2.0 * PI * 10.0 * t(i))   ! 示例信号
  end do
  
  ! 傅里叶变换
  do i = 1, N
    freq(i) = (0.0, 0.0)
    do j = 1, N
      freq(i) = freq(i) + signal(j) * exp(complex(0.0, -2.0 * PI * real(i-1) * real(j-1) / real(N)))
    end do
    freq(i) = freq(i) / real(N)
  end do
  
  ! 计算频谱
  do i = 1, N
    spectrum(i) = sqrt(real(freq(i))**2 + aimag(freq(i))**2)   ! 幅度谱
  end do
  
  ! 输出结果
  do i = 1, N
    write(*, '(2(F10.6, 2X))') t(i), signal(i)
  end do
  
  write(*, *) '---------------------------------'
  
  do i = 1, N
    write(*, '(2(F10.6, 2X))') real(i-1) / (N * dt), spectrum(i)
  end do

end program FourierTransform
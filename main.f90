

subroutine render_frame(A, B)
    implicit none
    real, intent(in) :: A
    real, intent(in) :: B

    real, parameter :: pi = 3.141592 
    real, parameter :: tau = pi*2

    integer, parameter :: screen_height = 30
    integer, parameter :: screen_width = screen_height * 3/2

    real, parameter :: theta_spacing = 0.07
    real, parameter :: phi_spacing   = 0.02

    real, parameter :: R1 = 1
    real, parameter :: R2 = 2
    real, parameter :: K2 = 5
    real, parameter :: K1 = screen_width*K2*3/(8*(R1+R2))

    ! buffers
    character(len=1) :: output(screen_width, screen_height) = ' '
    real ::            zbuffer(screen_width, screen_height) = 0.0


    real :: cosA
    real :: cosB
    real :: sinA
    real :: sinB

    real ::    theta
    real :: costheta
    real :: sintheta
    real ::    phi
    real :: cosphi
    real :: sinphi

    real :: circlex
    real :: circley

    real :: x
    real :: y
    real :: z
    real :: ooz

    integer :: xp
    integer :: yp

    real :: L
    integer :: lum_index

    integer :: j
    integer :: i

     output = ' '
    zbuffer = 0.0

    cosA = cos(A)
    cosB = cos(B)
    sinA = sin(A)
    sinB = sin(B)


    do theta = 0, tau, theta_spacing
        costheta = cos(theta)
        sintheta = sin(theta)

        do phi = 0, tau, phi_spacing
            cosphi = cos(phi)
            sinphi = sin(phi)

            circlex = R2 + R1*costheta
            circley =      R1*sintheta

            x = circlex*(cosB*cosphi + sinA*sinB*sinphi) - circley*cosA*sinB
            y = circlex*(sinB*cosphi - sinA*cosB*sinphi) + circley*cosA*cosB
            z = K2 + cosA*circlex*sinphi + circley*sinA
            ooz = 1/z
            
            xp = int(screen_width /2 + K1*ooz*x)
            yp = int(screen_height/2 + K1*ooz*y)

            L = cosphi*costheta*sinB - cosA*costheta*sinphi - sinA*sintheta + cosB*(cosA*sintheta - costheta*sinA*sinphi)

            if (xp <= 0.0) cycle
            if (yp <= 0.0) cycle
            if (xp >= screen_width)  cycle
            if (yp >= screen_height) cycle


            if (L > 0.0) then
                if (ooz > zbuffer(xp,yp)) then
                    zbuffer(xp,yp) = ooz
                    lum_index = int(L*8)+1
                    output(xp,yp) = ".,-~:;=!*#$@"(lum_index:lum_index)
                end if
            end if
            

        end do
    end do
            
    
    print *, (char(27) // '[H')
    do j = screen_height, 0, -1
        do i = 0, screen_width, 1
            write(*, '(A)', advance="no") output(i, j)
        end do
        print *, ''
    end do

end subroutine render_frame







program main
    use, intrinsic :: iso_fortran_env, only : int64
    implicit none
    
    integer(int64) :: clock_max, clock_count, clock_rate

    real :: i = 0
    real :: last_render = 0.0
    real :: now = 0.0
    real :: dt

    do
        call system_clock(clock_count, clock_rate, clock_max)
        now = real(clock_count) / real(clock_rate)
        dt = now - last_render

        if (dt < 0.1) cycle
        last_render = now
        

        call render_frame(i, i)
        i = i + 0.1
    end do

end program main

;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-modules (gnu packages shells)
             (gnu packages chromium)
             (nongnu packages linux)       ; nongnu channel
             (nongnu system linux-initrd)) ; nongnu channel
(use-service-modules desktop networking ssh xorg)

;;;; Custom definitions
(define %my-sudoers
   (plain-file "sudoers"
               "root ALL=(ALL) ALL\n%wheel ALL=(ALL) ALL\n@includedir /etc/sudoers.d\n"))

;; Add private substitute server
(define %custom-services
  (modify-services %desktop-services
    (guix-service-type config =>
      (guix-configuration (inherit config)
        (substitute-urls (cons "http://10.0.0.38:8181" %default-substitute-urls))
        (authorized-keys (cons (local-file "/etc/guix.d/erazer.pub") %default-authorized-guix-keys))))))

(define %my-packages
  '(
    ;; WM/Env
    "i3-gaps"
    "i3lock"
    "polybar"
    "feh"
    "kitty"
    "xmodmap"
    ;; Emacs
    "emacs-pgtk-native-comp" ;; flat channel
    "libvterm"
    "gcc-toolchain"
    "cmake"
    "make"
    "autoconf"
    "automake"
    ;; Misc
    "rlwrap"
    "nextcloud-client"
    "pasystray"
    "imagemagick"
    "stow"
    "git"
    "tig"
    "ripgrep"
    "bat"
    "fd"
    "fzf"
    "curl"
    "ncurses"
    "ncdu"
    "file"
    "ungoogled-chromium"
    "guile-hall"
    "nss-certs"))

;; System
(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Zurich")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (host-name "aoi")

  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")
      (keyboard-layout keyboard-layout)))

  ;; < nongnu
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (cons* iwlwifi-firmware %base-firmware))
  ;; > nongnu

  (swap-devices
    (list (uuid "6bff2c07-fad3-4920-a8a8-fa865aa39775")))

  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "1114202d-0c6f-425c-bc22-8d6b56978f0c"
                     'ext4))
             (type "ext4"))
           %base-file-systems))

  (users (cons* (user-account
                  (name "spike")
                  (comment "Spike")
                  (group "users")
                  (shell (file-append zsh "/bin/zsh"))
                  (home-directory "/home/spike")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  (sudoers-file %my-sudoers)

  (packages
    (append
     (map specification->package %my-packages)
     %base-packages))

  (services
    (append
      (list (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %custom-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))

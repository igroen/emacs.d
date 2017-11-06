;; Use indentatino of 4 spaces
(setq-default c-basic-offset 4)

;; Use irony-mode for c-hooks
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Enable company completion for irony
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; Enable flycheck for irony
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Enable irony-eldoc
(add-hook 'irony-mode-hook #'irony-eldoc)

;; Set gcc and clang c++ version used
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
;; (setq irony-additional-clang-options '("-std=c++14"))
;;
;; It's better to use a compilation database instead
;; ----------------------------------
;; Create a CMakeLists.txt file.
;; Example:
;;
;; project (MyProject)
;; cmake_minimum_required (VERSION 3.9)
;;
;; set(CMAKE_CXX_STANDARD 14)
;; set(CMAKE_CXX_STANDARD_REQUIRED ON)
;; set(CMAKE_CXX_EXTENSIONS OFF)
;; add_definitions(-Wall)
;;
;; add_executable(main main.cpp)
;;
;; find_package(Boost REQUIRED COMPONENTS program_options regex)
;; target_link_libraries(main ${Boost_LIBRARIES})
;;
;; To generate a JSON compilation database run:
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .

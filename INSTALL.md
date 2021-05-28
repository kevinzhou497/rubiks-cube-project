# Installation Instructions
## Setting Up GUIs
### Windows WSL 2

1. Install `x11-apps` and `mesa-utils` on WSL 2
2. Run the following commands, or add them to your .bashrc:
    ```
    export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
    export LIBGL_ALWAYS_INDIRECT=0
    ```

    *Note:* Unless you add these commands to your `.bashrc` you will have to execute them every time you exit the terminal session.
3. Allow the WSL connection through windows firewall
    
    Go to firewall -> advanced settings -> inbound rules -> new rule

    Select a Port rule on port 6000 (most likely)

    Allow the connection

    After the rule has been created, you can right click it and select properties. Then you can go to the scope tab and limit the Remote Ips from "any" to `172.16.0.0/12` I did not do this, but this would provide added security.

    For more information, view [this page](https://github.com/cascadium/wsl-windows-toolbar-launcher#firewall-rules)
3. Install an XServer on the Windows Host. I used [VcXsvr](https://sourceforge.net/projects/vcxsrv/)
4. Launch the XServer. VcXsvr has an option to *Disable Access Control* which you should check off. You should also uncheck *Native Opengl*
5. To test that it works, install the package `x11-apps` on WSL and run the command `xeyes`. If everything works, you should see eyes pop up on your windows host.
6. To test that opengl works, run the command `glxgears`. You should see gears pop up on the screen which turn at a normal rate. The command `glxinfo -B` should give you information, including your openGL core profile. You should see `"direct rendering: yes"` and an `"OpenGL core profile"` with a value greater than or equal to 3.3. If your hardware does not support OpenGL 3.3, this project will not run

For more information, check out [this SO post](https://stackoverflow.com/questions/61110603/how-to-set-up-working-x11-forwarding-on-wsl2)

### Windows WSL

1. Install `x11-apps` and `mesa-utils` on WSL
2. Run the command 
    ```
    export DISPLAY=:0
    export LIBGL_ALWAYS_INDIRECT=0
    ``` 
    or add it to your .bashrc

    *Note:* Unless you add these commands to your `.bashrc` you will have to execute them every time you exit the terminal session.
3. Install an XServer on windows such as [VcXsvr](https://sourceforge.net/projects/vcxsrv/) and run it. When you run it, uncheck *Native OpenGL*
4. To test that it works, install the package `x11-apps` on WSL and run the command `xeyes`. If everything works, you should see eyes pop up on your windows host.
5. To test OpenGL works, run the command `glxgears`. You should see gears pop up on the screen which turn at a normal rate (smooth). The command `glxinfo -B` should give you information, including your openGL core profile. You should see `"direct rendering: yes"` and an `"OpenGL core profile"` with a value greater than or equal to 3.3. If your hardware does not support OpenGL 3.3, this project will not run

### MAC, Linux, or Linux VM

See Dependencies and building instructions


## Dependencies

Run the following commands to install the following packages


```
apt install pkg-config libglfw3-dev libffi-dev zlib1g-dev
opam install tgls glfw-ocaml stb_image torch
```


## Building

Run the command `make main` to build and run the windowed program. The command `make demo` starts the terminal version.
For MAC Users: please run `make mac` instead of `make main`

See the README for controls
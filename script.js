const repourl = "https://github.com/matheusfillipe/myblog/blob/master/"

document.addEventListener("DOMContentLoaded", function() {
    // Mark current page
    const navElements = document.querySelectorAll(".topbar-menu a.current")
    navElements.forEach((a) => {
        const path = window.location.pathname
        a.classList.remove("current")
        a.classList.add("current-red")
        if (path === a.pathname) {
            a.classList.add("current")
            a.classList.remove("current-red")
        }
    })

    console.log("There is nothing for you to see here!")
    const nav = document.querySelector(".orgyeah-header");
    let lastScrollY = window.scrollY;

    const goTopBtn = document.getElementById("goTopBtn");
    goTopBtn.classList.add("bottombnt--hidden")
    goTopBtn.style.display = "block"
    const showBtnOffset = 400;

    window.addEventListener("scroll", () => {
        // Navbar
        if (lastScrollY < window.scrollY) {
            nav.classList.add("nav--hidden");
        } else {
            nav.classList.remove("nav--hidden");
        }
        lastScrollY = window.scrollY;

        // Go to Top Button
        if (document.body.scrollTop > showBtnOffset || document.documentElement.scrollTop > showBtnOffset) {
            goTopBtn.classList.remove("bottombnt--hidden")
        } else {
            goTopBtn.classList.add("bottombnt--hidden")
        }
    });

    //Replace github reference link on footer
    const gitref = document.querySelector("#githubref");
    if (!window.location.pathname.endsWith(".html")) {
        const directory = window.location.pathname.split("/").slice(0, -1).join("/")
        gitref.href = repourl + directory + "/index.org"
    } else {
        gitref.href = repourl + window.location.pathname.replace(".html", ".org")
    }

    //Make the terminal draggagle:
    dragElement(document.getElementById("terminalwindow"), "terminaltitlebar");
    function dragElement(elmnt, header) {
        var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
        if (document.getElementById(header)) {
            /* if present, the header is where you move the DIV from:*/
            document.getElementById(header).onmousedown = dragMouseDown;
        } else {
            /* otherwise, move the DIV from anywhere inside the DIV:*/
            elmnt.onmousedown = dragMouseDown;
        }

        function dragMouseDown(e) {
            e = e || window.event;
            e.preventDefault();
            // get the mouse cursor position at startup:
            pos3 = e.clientX;
            pos4 = e.clientY;
            document.onmouseup = closeDragElement;
            // call a function whenever the cursor moves:
            document.onmousemove = elementDrag;
        }

        function elementDrag(e) {
            e = e || window.event;
            e.preventDefault();
            // calculate the new cursor position:
            pos1 = pos3 - e.clientX;
            pos2 = pos4 - e.clientY;
            pos3 = e.clientX;
            pos4 = e.clientY;
            // set the element's new position:
            elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
            elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
        }

        function closeDragElement() {
            /* stop moving when mouse button is released:*/
            document.onmouseup = null;
            document.onmousemove = null;
        }
    }


    const terminal = document.querySelector("#terminalwindow")

    // Scroll inside terminal div
    function scrollToView(element) {
        var offset = element.offset().top;
        var visible_area_start = $(window).scrollTop();
        var visible_area_end = visible_area_start + window.innerHeight;

        if (offset < visible_area_start || offset > visible_area_end) {
            // Not in view so scroll to it
            $('html,body').animate({ scrollTop: offset - window.innerHeight / 3 }, 300);
            return false;
        }
        return true;
    }

    $('#terminal').bind('wheel', e => {
        const oEvent = e.originalEvent
        const delta = oEvent.deltaY || oEvent.wheelDelta
        terminal.scrollBy(0, delta / 10)
    })
    // Terminal buttons
    const minimizebtn = document.querySelector("#terminimize")

    function showterm() {
        terminal.classList.remove("terminal--hidden")
        setTimeout(function() {
            $('#terminal').terminal().focus()
            scrollToView($("#terminal"))
        }, 500);
    }
    minimizebtn.onclick = () => {
        terminal.classList.add("terminal--hidden")
    }
    const showterminal = document.querySelector("#showterminal")
    showterminal.onclick = () => {
        if (terminal.classList.contains("terminal--hidden")) {
            showterm()
            return
        }
        terminal.classList.add("terminal--hidden")
    }
});

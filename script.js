const repourl = "https://github.com/matheusfillipe/myblog/blob/master/"

document.addEventListener("DOMContentLoaded", function(){
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
        const directory = window.location.pathname.split("/").slice(0,-1).join("/")
        gitref.href = repourl + directory + "index.org"
    } else {
        gitref.href = repourl + window.location.pathname.replace(".html", ".org")
    }
});

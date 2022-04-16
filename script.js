// Enable hidden nav bar
document.addEventListener("DOMContentLoaded", function(){
    console.log("There is nothing for you to see here!")
    const nav = document.querySelector(".orgyeah-header");
    let lastScrollY = window.scrollY;
    window.addEventListener("scroll", () => {
        if (lastScrollY < window.scrollY) {
            nav.classList.add("nav--hidden");
        } else {
            nav.classList.remove("nav--hidden");
        }

        lastScrollY = window.scrollY;
    });

});

function getsvg(event) {
    let t = event.target;
    while(t && t.tagName.toLowerCase()!='svg') {
      t = t.parentElement;
    }
    return t;
}

function getcoords(event) {
    const t = getsvg(event);
    const point = t.createSVGPoint()
    point.x = event.clientX
    point.y = event.clientY
    const position = point.matrixTransform(t.getScreenCTM().inverse())                
    return position;
}

var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
        if (mutation.type === 'childList') {
            Array
                .from(mutation.addedNodes)
                .forEach(function (node) {
                    node.addEventListener('mousemove', function (event) {
                        const t = getsvg(event);
                        if(!t) {
                            return;
                        }
                        const position = getcoords(event);
                        const svgMoveEvent = new CustomEvent('svgmove', {
                            detail: {x: position.x, y: position.y},
                        });
                        t.dispatchEvent(svgMoveEvent);
                    });
                    function svg_touch_event(name) {
                        node.addEventListener(name, function(event) {
                            const t = getsvg(event);
                            if(!t) {
                                return;
                            }
                            event.preventDefault();
                            event.stopPropagation();
                            const touches = Array.from(event.changedTouches).map(touch => {
                                const position = getcoords(touch);
                                return {identifier: touch.identifier, position: position}
                            });
                            const touchEvent = new CustomEvent('svg'+name, {
                                detail: touches
                            });
                            t.dispatchEvent(touchEvent);
                        });
                    };
                    ['touchstart','touchmove','touchend'].forEach(svg_touch_event)
                });
        }
    });
});

document.addEventListener('mousemove',e => {
    const f = document.activeElement;
    if(f && getsvg(f)) {
        f.blur();
    }
})

class JiggraphAppElement extends HTMLElement {
    constructor() {
        super();

        const key = 'jiggraph'+(this.getAttribute('id' || ''));
        const data = JSON.parse(localStorage.getItem(key));

        var app = Elm.Main.init({ node: this, flags: data })

        app.ports.saveLocalStorage.subscribe(data => {
            localStorage.setItem(key, JSON.stringify(data));
        });
    }
}

observer.observe(document.body, { childList: true, subtree: true });    

customElements.define('jiggraph-app', JiggraphAppElement);


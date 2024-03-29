fetch('/error.txt').then(r=>{
    if(r.ok) {
        return r.text();
    } else {
        throw('');
    }
}).then(text => {
    if(!text) {
        return;
    }
    document.body.innerHTML = '';
    const error_show = document.createElement('pre');
    error_show.setAttribute('id','build-error');
    error_show.style.background = 'black';
    error_show.style.color = 'white';
    error_show.style.padding = '1em';
    error_show.textContent = text;
    document.body.appendChild(error_show);
}).catch(e=>{});

let observer = null

function jsMain() {
  observer = new MutationObserver(onMutationObserved);
  observer.observe(document.querySelector('body'), { attributeFilter: ['class'], attributes: true, childList: true, subtree: true });
}

function onMutationObserved(mutationList, observer) {
  mutationList.forEach(record => {
    if (record.type === "childList") {
      const elements = document.querySelectorAll('.imdc-to-init');
      elements.forEach(elem => {
        const elemType = elem.dataset.imdc
        switch (elemType) {
          // case "TextField":
          //   initMDCTextField(elem);
          //   break;
          case "RippleButton":
            initMDCRippleButton(elem);
            break;
        }
      });
    }
  });
}

function initMDCTextField(elem) {
  setTimeout(() => {
    const mdcValue = mdc.textField.MDCTextField.attachTo(elem);
    postInitMDC(mdcValue);
  }, 300)
}

function initMDCRippleButton(elem) {
  const mdcValue = mdc.ripple.MDCRipple.attachTo(elem);
  postInitMDC(elem, mdcValue);
}

function postInitMDC(elem, mdcValue) {
  elem.dataset.imdc = mdcValue;
  elem.classList.remove('imdc-to-init');
  elem.classList.add('imdc-initialized');
}

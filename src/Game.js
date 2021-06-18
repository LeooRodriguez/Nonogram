import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.restart(true)
  }



  handlePengineCreate() {
    const queryS = 'init(PistasFilas, PistasColumns, Grilla)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grilla'],
          rowClues: response['PistasFilas'],
          colClues: response['PistasColumns'],
          satisfaccionFil: Array(response['PistasFilas'].length).fill(false), //inicializamos la fila para verificar en falso
          satisfaccionCol: Array(response['PistasColumns'].length).fill(false) //inicializamos la columna para verificar en falso
        }, ()=>this.cargarGrillaRes());
      }

    });

  }
/**
 * Carga la grilla resuelta del nonograma en el estado gridSol.
 */
  cargarGrillaRes() {

    const PF = this.state.rowClues.length;
    const PC = this.state.colClues.length;
    const rowClue = JSON.stringify(this.state.rowClues);
    const colClue = JSON.stringify(this.state.colClues);
    const queryX = 'resolverNonograma(' + PF + ',' + PC + ', ' + rowClue + ', ' + colClue + ', GrillaSo)';//Le pedimos a Prolog la grilla resuelta.
    this.pengine.query(queryX, (success, response) => {
      if (success) {
        let GrillaSol = response['GrillaSo'];
        this.setState({
          gridSol: GrillaSol
        });
      } else {
        this.setState({
          waiting: false
        });
      }
    });

  }

  handleClick(i, j) {
    // No action on click if we are waiting.
    if (this.state.waiting || this.state.modoResuelto || this.state.gridSol===null) {
      return;
    }

    const elem = this.state.gridSol[i][j];
    let modo;
    if(this.state.modoAyudita){
      if(this.state.grid[i][j]==="_"){//Según los requerimientos solo se debe poner ayudas en lugares vacios.
          modo= elem;
          this.llamarAPut(i,j,elem);
      }
    }
    else
      this.llamarAPut(i,j,this.state.modoBoton);
  }
/**
 * Consulta a Prolog para insertar un elemento en la grilla
 * @param {*} i Posición de la fila en la grilla
 * @param {*} j Posición de la columna en la grilla
 * @param {*} elem Elemento a insertar en la grilla (# ó X)
 */
  llamarAPut(i,j,elem){
    const squaresS = JSON.stringify(this.state.grid).replaceAll('"_"', "_"); // Remove quotes for variables.
    const rowClue = JSON.stringify(this.state.rowClues);
    const colClue = JSON.stringify(this.state.colClues);
    const queryS = 'put("' + elem + '", [' + i + ',' + j + ']' + ', ' + rowClue + ', ' + colClue + ',' + squaresS + ', GrillaRes, FilaSat, ColSat)';
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        let newGrid = response['GrillaRes'];
        let satisfaceFil = response['FilaSat'];
        let satisfaceCol = response['ColSat'];
        this.setState({
          grid: newGrid
        });
        this.confirmarFilaSatisfecha(i, satisfaceFil === 1);
        this.confirmarColSatisfecha(j, satisfaceCol === 1);

      } else {
        this.setState({
          waiting: false
        });
      }
    });
  }

  /**
   * Verifica si la columna con el indice index verifica la propiedad.
   * @param {*} index es el indice de la columna que deseamos verificar 
   * @param {*} satisface indica si se satisfacio la columna 
   */
  confirmarColSatisfecha(index, satisface) {
    let satisfaccionCol = [...this.state.satisfaccionCol];
    satisfaccionCol[index] = satisface;
    this.setState({ satisfaccionCol, waiting: false });
  }

  /**
   * Verifica si la fila con el indice index verifica la propiedad.
   * @param {*} index es el indice de la fila que deseamos verificar 
   * @param {*} satisface indica si se satisfacio la fila
   */

  confirmarFilaSatisfecha(index, satisface) {
    let satisfaccionFil = [...this.state.satisfaccionFil];
    satisfaccionFil[index] = satisface;
    this.setState({ satisfaccionFil, waiting: false });
  }
  /**
   * Cambia el estado del boton, alternando entre el # y la X según cual sea su estado actual.
   */
  cambiar() {
    if (this.state.modoBoton === "#") {
      this.setState({ modoBoton: "X" })
    }
    else
      this.setState({ modoBoton: "#" })
  }
/**
   * Cambia el estado del boton, alternando entre el True y el False según cual sea su estado actual.
   */
  resolver() {
    if (this.state.modoResuelto == false)
      this.setState({ modoResuelto: true })
    else
      this.setState({ modoResuelto: false })
  }

/**
   * Cambia el estado del boton, alternando entre el True y el False según cual sea su estado actual.
   */
  ayuda() {
    if (this.state.modoAyudita == false)
      this.setState({ modoAyudita: true })
    else
      this.setState({ modoAyudita: false })
  }

  abrirManual() {
    window.open("https://es.puzzle-nonograms.com/faq.php", "Manual de usuario / Tutorial", "width=800, height=700")
  }
  irGithub() {
    window.open("https://github.com/LeooRodriguez/Nonogram", "GitHub", "width=800, height=700")
  }


  render() {
    if (this.state.grid === null) {
      return null;
    }



    let { satisfaccionFil, satisfaccionCol } = this.state;

    if (satisfaccionFil.every(a => a) && satisfaccionCol.every(a => a)) {//Si todas las filas y columnas de la grilla son true --> ganaste el juego.
      return <div>
        <div className={"cartelGanaste"}>¡FELICIDADES,HAS GANADO!</div>

        <botonesGenerales className={"botonDeReset"} onClick={() => this.restart()}>
          Pulse para comenzar otra partida
        </botonesGenerales>

        <botonesGenerales className={"botonDeGit"} onClick={() => this.irGithub()}>
          Código del nonograma (GitHub)
        </botonesGenerales>
      </div>
    }

    return (

      
      <div className="game">
        <Board
          grid={this.state.modoResuelto&&this.state.gridSol!=null ? this.state.gridSol : this.state.grid}
          rowClues={this.state.rowClues}
          colClues={this.state.colClues}
          onClick={(i, j) => this.handleClick(i, j)}
          satisfaccionFil={this.state.satisfaccionFil}
          satisfaccionCol={this.state.satisfaccionCol}
        />
        <div>
          <button className="box clue nsat" onClick={() => this.cambiar()} >
            {this.state.modoBoton}
          </button>
          <div className={"cartelNombre"}>Nonograma
          </div>
          <div className={"Estado"}>
          </div>
          <botonesGenerales className={"botonReglas"} onClick={() => this.abrirManual()}>
            Reglas
          </botonesGenerales>
          <botonesGenerales className={"botonDeReinicio"} onClick={() => this.restart()}>
            Reiniciar
          </botonesGenerales>

          <div>
        <input type = "checkbox" className = "checkboxResolverNonograma" id = "checkboxResolverNonograma" onChange = {() => this.resolver()} value = {this.state.modoResuelto} ></input>
           <label htmlFor = "checkboxResolverNonograma" className = "labelResolverNonograma">
           <i className ="fa fa-question" aria-hidden="true"></i>
           <div className = "ballResolverNonograma"></div>
           </label>
       </div>

       <div>
        <input type = "checkbox" className = "checkboxAyudita" id = "checkboxAyudita" onChange = {() => this.ayuda()} value = {this.state.modoAyudita} ></input>
           <label htmlFor = "checkboxAyudita" className = "labelAyudita">
           <i className ="fa fa-question" aria-hidden="true"></i>
           <div className = "ballAyudita"></div>
           </label>
       </div>
       
       <div className={"cartelAyuda"}>Ayuda
          </div>

          <div className={"cartelResolver"}>Resolver
          </div>

        </div>
      </div>

    );
  }


  restart(isFirstTime = false) {
    this.state = {
      grid: null,
      gridSol: null,
      rowClues: null,
      colClues: null,
      waiting: false,
      satisfaccionFil: [],//Guarda verdaderos o falsos dependiendo si la filas cumple las propiedades del nonograma. 
      satisfaccionCol: [],//Guarda verdaderos o falsos dependiendo si la columnas cumple las propiedades del nonograma.
      modoBoton: "#",//Verifica en que estado esta el botón ( # ó X).
      modoResuelto: false,//Modo para alternar la grilla resuelta.
      modoAyudita: false,//Modo para ayudar al usuario.
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    if (isFirstTime) {
      this.pengine = new PengineClient(this.handlePengineCreate);
    }
    else {
      this.handlePengineCreate()//Inicia una nueva partida ya mismo.
    }
  }
}


















export default Game;



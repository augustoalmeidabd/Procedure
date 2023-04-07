-- Start of DDL Script for Package Body TESTE.PKG_GERAFINANFATURAMENTO_FT51
-- Generated 14-fev-2023 19:16:48 from TESTE@TESTE

CREATE OR REPLACE 
PACKAGE BODY pkg_gerafinanfaturamento_ft51 IS



  PROCEDURE validarPcprazoAdicional(
                                    --Parametros de entrada
                                    pdtdata        IN DATE,
                                    pdnumregiao    IN pcregiao.numregiao%TYPE,
                                    pdcodcli       IN pcclient.codcli%TYPE,
                                    pdcodusur      IN pcusuari.codusur%TYPE,
                                    psorigempedido IN pcpedc.origemped%TYPE,
                                    pdcodplpag     IN pcplpag.codplpag%TYPE,
                                    pnvltotal      IN pcnfsaid.vltotal%TYPE,
                                    pncodcidade    IN pcclient.codcidade%TYPE,
                                    pscodfilial    IN pcfilial.codigo%TYPE,
                                    --Parametros de Saida
                                    psmensagem OUT VARCHAR2,
                                    pdnumdias  OUT pcprazoadicional.numdias%TYPE)
  /******************************************************************************************
    ->Nome da PROCEDURE : Validar_PCPRAZOADICIONAL
    ->Objetivo          : Faturar pedidos nao faturados.
    ->Versao            : Pacote
    ->Utilização        : Procedure de Faturamento (fatu_fatura_pedido) e rotina 317

    -------------------------------- Historico ------------------------------------------------------------------
    Data          Responsavel       Tarefa   Comentarios
    ------------  ---------------   -------- --------------------------------------------------------------------
    05/03/2007    Pablo             39450    Procedure criada para retornar o NUMERO de DIAS do Prazo Adicional
                                             (PCPRAZOADICIONA) rotina PCSIS397
    21/03/2007    Pablo                      Procedure passa a ser criada pela rotina PCSIS500
    28/03/2007    Pablo             41462    Procedure também será criada pela rotina 1400
    09/02/2012    Thiago            155890   Solicitação Scrum Master Pablo.
    **************************************************************************************************************************/
   IS
    --Dados do Cliente
    vdcodcliprinc pcclient.codcliprinc%TYPE;
    vdcodatv1     pcclient.codatv1%TYPE;
    vdcodpraca    pcclient.codpraca%TYPE;
    --Dados do RCA
    vdcodsupervisor pcsuperv.codsupervisor%TYPE;
    --Variaveis de Controle
    vncontador    NUMBER;
    pvc2menssagen VARCHAR2(255);
  BEGIN
    pdnumdias     := 0;
    pvc2menssagen := 'OK';

    BEGIN
      -- Buscar dados do cliente
      BEGIN
        SELECT NVL(codcliprinc, codcli) codcliprinc, codatv1, codpraca
          INTO vdcodcliprinc, vdcodatv1, vdcodpraca
          FROM pcclient
         WHERE codcli = pdcodcli;
      EXCEPTION
        WHEN OTHERS THEN
          pvc2menssagen := SQLCODE || '-' || SQLERRM ||
                           '22.00 - Não foi possível buscar dados do PCCLIENT! ' ||
                           ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
          RAISE;
      END;

      -- Buscar dados do RCA
      BEGIN
        SELECT codsupervisor
          INTO vdcodsupervisor
          FROM pcusuari
         WHERE codusur = pdcodusur;
      EXCEPTION
        WHEN OTHERS THEN
          pvc2menssagen := SQLCODE || '-' || SQLERRM ||
                           '22.01 - Não foi possível buscar dados do PCUSUARI! ' ||
                           ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
          RAISE;
      END;

      -- Verificação de Políticas de Descontos

      -- Verifica Tabela de Prazo Adicional (sem rede de clientes)
      SELECT COUNT(*)
        INTO vncontador
        FROM pcprazoadicional
       WHERE pdtdata BETWEEN dtinicio AND dtfim
         AND NVL(utilizadescrede, 'N') = 'N'
         AND ((codusur = pdcodusur) OR (codusur IS NULL))
         AND ((codsupervisor = vdcodsupervisor) OR (codsupervisor IS NULL))
         AND ((codcli = pdcodcli) OR (codcli IS NULL))
         AND ((numregiao = pdnumregiao) OR (numregiao IS NULL))
         AND ((codativ = vdcodatv1) OR (codativ IS NULL))
         AND ((origemped = psorigempedido) OR (NVL(origemped, 'O') = 'O'))
         AND ((codpraca = vdcodpraca) OR (codpraca IS NULL))
         AND ((codplpag = pdcodplpag) OR (codplpag IS NULL))
         AND ((codfilial = pscodfilial) OR (codfilial IS NULL))
         AND ((codcidade = pncodcidade) OR (codcidade IS NULL))
         AND ((NVL(vlminvenda, 0) <= pnvltotal))
       ORDER BY numdias DESC;

      IF vncontador > 0 THEN
        SELECT NVL(numdias, 0)
          INTO pdnumdias
          FROM pcprazoadicional
         WHERE pdtdata BETWEEN dtinicio AND dtfim
           AND NVL(utilizadescrede, 'N') = 'N'
           AND ((codusur = pdcodusur) OR (codusur IS NULL))
           AND ((codsupervisor = vdcodsupervisor) OR
               (codsupervisor IS NULL))
           AND ((codcli = pdcodcli) OR (codcli IS NULL))
           AND ((numregiao = pdnumregiao) OR (numregiao IS NULL))
           AND ((codativ = vdcodatv1) OR (codativ IS NULL))
           AND ((origemped = psorigempedido) OR (NVL(origemped, 'O') = 'O'))
           AND ((codpraca = vdcodpraca) OR (codpraca IS NULL))
           AND ((codplpag = pdcodplpag) OR (codplpag IS NULL))
           AND ((codfilial = pscodfilial) OR (codfilial IS NULL))
           AND ((codcidade = pncodcidade) OR (codcidade IS NULL))
           AND ((NVL(vlminvenda, 0) <= pnvltotal))
           AND ROWNUM = 1
         ORDER BY numdias DESC;
      END IF;

      -- Verifica Tabela de Prazo Adicional da Rede de Clientes //
      SELECT COUNT(*)
        INTO vncontador
        FROM pcprazoadicional
       WHERE codcli = vdcodcliprinc
         AND pdtdata BETWEEN dtinicio AND dtfim
         AND NVL(utilizadescrede, 'N') = 'S'
         AND ((codusur = pdcodusur) OR (codusur IS NULL))
         AND ((codsupervisor = vdcodsupervisor) OR (codsupervisor IS NULL))
         AND ((numregiao = pdnumregiao) OR (numregiao IS NULL))
         AND ((codativ = vdcodatv1) OR (codativ IS NULL))
         AND ((origemped = psorigempedido) OR (NVL(origemped, 'O') = 'O'))
         AND ((codpraca = vdcodpraca) OR (codpraca IS NULL))
         AND ((codplpag = pdcodplpag) OR (codplpag IS NULL))
         AND ((codfilial = pscodfilial) OR (codfilial IS NULL))
         AND ((codcidade = pncodcidade) OR (codcidade IS NULL))
         AND ((NVL(vlminvenda, 0) <= pnvltotal))
       ORDER BY numdias DESC;

      IF vncontador > 0 THEN
        SELECT NVL(numdias, 0)
          INTO pdnumdias
          FROM pcprazoadicional
         WHERE codcli = vdcodcliprinc
           AND pdtdata BETWEEN dtinicio AND dtfim
           AND NVL(utilizadescrede, 'N') = 'S'
           AND ((codusur = pdcodusur) OR (codusur IS NULL))
           AND ((codsupervisor = vdcodsupervisor) OR
               (codsupervisor IS NULL))
           AND ((numregiao = pdnumregiao) OR (numregiao IS NULL))
           AND ((codativ = vdcodatv1) OR (codativ IS NULL))
           AND ((origemped = psorigempedido) OR (NVL(origemped, 'O') = 'O'))
           AND ((codpraca = vdcodpraca) OR (codpraca IS NULL))
           AND ((codplpag = pdcodplpag) OR (codplpag IS NULL))
           AND ((codfilial = pscodfilial) OR (codfilial IS NULL))
           AND ((codcidade = pncodcidade) OR (codcidade IS NULL))
           AND ((NVL(vlminvenda, 0) <= pnvltotal))
           AND ROWNUM = 1
         ORDER BY numdias DESC;
      END IF;
      --Fim da função
    EXCEPTION
      WHEN OTHERS THEN
        pvc2menssagen := SQLCODE || '-' || SQLERRM ||
                         '22.02 -Não possível validar o prazo Adicional (PCPRAZOADICIONAL) ' ||
                         ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
        RAISE;
    END;

    --Retorno da função
    psmensagem := pvc2menssagen;
  END validarpcprazoadicional;

  /*
  Autor      : Rafael Braga
  Solicitação: HIS.00023.2017
  Data       : 02/02/2017
  Descrição  : Colocando nota na fila da SupplierCard caso tenha sido usado
  */
  procedure gravarFilaSuppli(pPlanoSuppli     in number,
                             pCondfinanc      in varchar2,
                             PN_NUMTRANSVENDA IN NUMBER) is
    vRowFilaNormal   pcfilasuppli%rowtype;
    vRowFilaDesconto pcfilasuppli%rowtype;
    --vDtVenc1         date;
    --vDtVenc2         date;
    vdValorDesc number;

    function getDtVenc(pPrest in number) return Date is
      vdDataVenc date;
    begin
      begin
        select dtvenc
          into vdDataVenc
          from pcprest
         where numtransvenda = PN_NUMTRANSVENDA
           and prest = pPrest;
      exception
        when others then
          vdDataVenc := null;
      end;
      return(vdDataVenc);
    end;

  begin

    vRowFilaNormal.Dtvenc1       := getDtVenc(1);
    vRowFilaNormal.Dtvenc2       := getDtVenc(2);
    vRowFilaNormal.tipoinf       := 1;
    vRowFilaNormal.Condfinanc    := pCondfinanc;
    vRowFilaNormal.plano         := pPlanoSuppli;
    vRowFilaNormal.dthorainc     := sysdate;
    vRowFilaNormal.Codrotinacad  := 1400;
    vRowFilaNormal.Enviadosuppli := 'N';
    vRowFilaNormal.numtransvenda := PN_NUMTRANSVENDA;

    insert into pcfilasuppli values vRowFilaNormal;

    begin
      select sum(nvl(valordesc, 0)) valorDesc
        into vdValorDesc
        from pcprest
       where numtransvenda = PN_NUMTRANSVENDA
         and nvl(valordesc, 0) > 0;
    exception
      when others then
        vdValorDesc := 0;
    end;

    FOR PREST IN (select nvl(valordesc, 0) valorDesc, prest
                   from pcprest
                  where numtransvenda = PN_NUMTRANSVENDA
                    and nvl(valordesc, 0) > 0)
    LOOP
      --vRowFilaDesconto.Dtvenc1       := getDtVenc(1);
      --vRowFilaDesconto.Dtvenc2       := getDtVenc(2);
      vRowFilaDesconto.numtransvenda := PN_NUMTRANSVENDA;
      vRowFilaDesconto.tipoinf       := 11;
      vRowFilaDesconto.Condfinanc    := pCondfinanc;
      vRowFilaDesconto.plano         := pPlanoSuppli;
      vRowFilaDesconto.dthorainc     := sysdate;
      vRowFilaDesconto.Codrotinacad  := 1400;
      vRowFilaDesconto.Enviadosuppli := 'N';
      vRowFilaDesconto.dtdescfin     := trunc(sysdate);
      vRowFilaDesconto.vldescfin     := PREST.valorDesc;
      vRowFilaDesconto.prest         := prest.prest;

      insert into pcfilasuppli values vRowFilaDesconto;

    END LOOP;
  end gravarFilaSuppli;

  /**********************************************************************************
  lucas.lima         | 2065.067449.2014 | 30/06/2014 | Adicionado parâmetro PNUMREGIAO;
  guilherme.freitas  | 207547           | 25/03/2015 | Adicionado NVL na busca das campanhas da 3327
  **********************************************************************************/
  FUNCTION F_BUSCARDESCFIN(PCODPROD           NUMBER,
                           PCODCLI            NUMBER,
                           PCODFILIAL         VARCHAR2,
                           PNUMREGIAO         NUMBER,
                           PUSADESCFINANCEIRO VARCHAR2,
                           PORIGEMPED         VARCHAR2,
                           PMAXDESC           OUT NUMBER,
                           PCODCAMP           OUT NUMBER) RETURN NUMBER IS

    vnNUMREGIAO pcregiao.numregiao%TYPE;
    vnCODEPTO   pcdepto.codepto%TYPE;
    vnCODSEC    pcsecao.codsec%TYPE;
    vnCODFORNEC pcfornec.codfornec%TYPE;
    vnDESCFIN   NUMBER;

  BEGIN

    vnNUMREGIAO := PNUMREGIAO;

    /* lucas.lima | 2065.067449.2014 | 30/06/2014
    Comentado o select abaixo pois dava problemas quando o usuário utilizasse tabela
    de preço por região informados no cadastro do cliente, PCTABPRCLI.

    SELECT numregiao
      INTO vnNUMREGIAO
      FROM pcpraca
     WHERE codpraca = (SELECT codpraca FROM pcclient WHERE codcli = PCODCLI);*/

    SELECT codepto, codsec, codfornec
      INTO vnCODEPTO, vnCODSEC, vnCODFORNEC
      FROM pcprodut
     WHERE codprod = PCODPROD;

    BEGIN
      SELECT A.* INTO vnDESCFIN, PCODCAMP, PMAXDESC
     FROM
    (
      SELECT nvl(max(perdesc), 0), codpoliticadesc, nvl(vlmaxdesconto,0) -- Adicionado o NVL, pois se não tinha % na 3327, fica nulo, e logo não buscava o do cliente 207547
        FROM pcpoliticadescfinan
       WHERE trunc(SYSDATE) BETWEEN dtinicio AND dtfim  AND
              ((CODPRODUTO = PCODPROD OR CODPRODUTO IS NULL) AND
              (CODCLIENTE = PCODCLI OR CODCLIENTE IS NULL) AND
              (CODFILIAL = PCODFILIAL OR CODFILIAL IS NULL) AND
              (NUMREGIAO = vnNUMREGIAO OR NUMREGIAO IS NULL) AND
              (CODFORNEC = vnCODFORNEC OR CODFORNEC is NULL) AND
              (CODEPTO = vnCODEPTO OR CODEPTO IS NULL) AND
              (CODSEC = vnCODSEC OR CODSEC IS NULL)) AND
              ((NVL(VENDABALCAO, 'S') = 'S' AND PORIGEMPED = 'B') OR
               (NVL(VENDABALCAORESERVA, 'S') = 'S' AND PORIGEMPED = 'R') OR
               (NVL(VENDATELEMARK, 'S') = 'S' AND PORIGEMPED = 'T')  OR
               (NVL(VENDACALLCENTER, 'S') ='S' AND PORIGEMPED = 'C') OR
               (NVL(VENDAFV, 'S') = 'S' AND PORIGEMPED = 'F'))
             --  AND ROWNUM = 1
               group by codpoliticadesc, vlmaxdesconto
               order by nvl(max(perdesc), 0)
               DESC
               )A
               where rownum = 1; -- VEN-8838 // Alterada a select para atribuir o 'rownum' fora da select.
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        vnDESCFIN := 0;
    END;

    IF NVL(vnDESCFIN, 0) = 0 THEN

      SELECT NVL(PCCLIENT.PERDESCFIN, 0)
        INTO vnDESCFIN
        FROM PCCLIENT
       WHERE CODCLI = PCODCLI;

      IF NVL(PUSADESCFINANCEIRO, 'N') = 'N' THEN
        vnDESCFIN := 0;
      END IF;

    END IF;

    RETURN NVL(vnDESCFIN, 0);

  EXCEPTION
    WHEN NO_DATA_FOUND THEN

      RETURN NULL;

  END F_BUSCARDESCFIN;

  FUNCTION FUNC_DIASEMANAVALIDO(PCODFILIAL    IN VARCHAR2,
                                PDATA         IN DATE,
                                PCONSIDERAFDS IN NUMBER,
                                PCODCLI       IN NUMBER) RETURN DATE IS
    V_DIAFINANCEIRO    DATE;
    V_USADIAUTILFILIAL VARCHAR2(2);
    --V_CONTADOR         NUMBER;
    V_DIASEMANA NUMBER;
    --V_DIAUTIL          VARCHAR(2);
    vbVALIDO boolean;
    --vnCODDIA             NUMBER;

    TYPE T_DIASSEMANA IS TABLE OF PCDIASEMANAPAGCLI%ROWTYPE;
    TB_DIASSEMANA T_DIASSEMANA;

  BEGIN
    BEGIN
      --V_CONTADOR := 1;
      --V_DIAUTIL  := '';

      --Obtendo Parâmentros
      V_USADIAUTILFILIAL := PARAMFILIAL.OBTERCOMOVARCHAR2('FIL_USADIAUTILFILIAL',
                                                          PCODFILIAL);
      V_DIAFINANCEIRO    := PDATA;
      vbVALIDO           := false;
      WHILE NOT (vbVALIDO) LOOP

        IF V_USADIAUTILFILIAL = 'S' THEN
          SELECT NVL(MIN(DATA), V_DIAFINANCEIRO) DATA
            INTO V_DIAFINANCEIRO
            FROM PCDIASUTEIS
           WHERE DATA >= V_DIAFINANCEIRO
             AND CODFILIAL = PCODFILIAL
             AND ((TO_CHAR(DATA, 'D') NOT IN (1, 7)) OR --Desconsidera Sábado(7) e Domingo(1)
                 PCONSIDERAFDS = 1)
             AND DIAFINANCEIRO = 'S';
        ELSE
          SELECT NVL(MIN(DATA), V_DIAFINANCEIRO) DATA
            INTO V_DIAFINANCEIRO
            FROM PCDATAS
           WHERE DATA >= V_DIAFINANCEIRO
             AND ((TO_CHAR(DATA, 'D') NOT IN (1, 7)) OR --Desconsidera Sábado(7) e Domingo(1)
                 PCONSIDERAFDS = 1)
             AND DIAUTIL = 'S';
        END IF;

        SELECT TO_CHAR(V_DIAFINANCEIRO, 'D') INTO V_DIASEMANA FROM DUAL;

        OPEN C_DIASSEMANA(PCODCLI);

        LOOP
          FETCH C_DIASSEMANA BULK COLLECT
            INTO TB_DIASSEMANA LIMIT 200;

          FOR i in 1 .. TB_DIASSEMANA.count() LOOP
            IF (TB_DIASSEMANA(I).CODDIA = V_DIASEMANA) THEN
              vbVALIDO := true;
            END IF;
          END LOOP;

          IF (TB_DIASSEMANA.count = 0) AND NOT (vbVALIDO) THEN
            vbVALIDO        := TRUE;
            V_DIAFINANCEIRO := PDATA;
          ELSE
            IF (NOT (vbVALIDO)) THEN
              V_DIAFINANCEIRO := V_DIAFINANCEIRO + 1;
            END IF;
          END IF;

          EXIT WHEN C_DIASSEMANA%NOTFOUND;
        END LOOP;

        CLOSE C_DIASSEMANA;

      END LOOP;

    END;
    RETURN V_DIAFINANCEIRO;
  END;

  ------
  -- CARREGAR_DTVENCIMENTO  - 4 parametros
  PROCEDURE CARREGAR_DTVENCIMENTO(pddtemissao  IN DATE,
                                  pncarencia   IN NUMBER,
                                  pndiafixo    IN NUMBER,
                                  pnumparcelas IN NUMBER) IS
    vnteste_diafixo   NUMBER(1);
    vddata_vencimento DATE;
    vndiafixo         NUMBER(2);
    vnmes             NUMBER(2);
    vb_diavalido      BOOLEAN;
    vnverifmesatual   NUMBER(3);
  BEGIN
    vb_diavalido    := FALSE;
    vnmes           := 0;
    vnverifmesatual := 0;
    G_DATAS.DELETE;

    FOR N IN 1 .. pnumparcelas LOOP

      G_DATAS.EXTEND;
      G_DATAS(N).PREST := TO_CHAR(N);
      vnteste_diafixo := 0;
      vnmes := vnmes + 1;
      vb_diavalido := FALSE;
      vndiafixo := pndiafixo;
      WHILE (NOT vb_diavalido) LOOP
        <<TESTE_DATA>>
        WHILE vnteste_diafixo = 0 LOOP
          BEGIN
            IF (N = 1) AND (vnverifmesatual = 0) THEN
              --Se for a primeira parcela, verifica se o dia atual é menor que o dia fixo
              IF EXTRACT(DAY FROM SYSDATE) < vndiafixo THEN
                vnmes           := 0;
                vnverifmesatual := vnverifmesatual + 1;
              END IF;
            END IF;

            SELECT TO_DATE(TO_CHAR(vndiafixo) || '/' ||
                           TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE), vnmes),
                                   'MM/YYYY'),
                           'DD/MM/YYYY')
              INTO vddata_vencimento
              FROM DUAL;

            vnteste_diafixo := 1;
          EXCEPTION
            WHEN OTHERS THEN
              vndiafixo := vndiafixo - 1;
          END;
        END LOOP;

        IF ((pddtemissao + pncarencia) < vddata_vencimento) THEN
          G_DATAS(N).DTVENCIMENTO := vddata_vencimento;
          vb_diavalido := TRUE;
        ELSE
          IF (N = 1) THEN
            --Se a primeira parcela + carencia for invalida o dia fixo volta ao original e incrementa o mes.
            vndiafixo := pndiafixo;
          END IF;
          vnmes           := vnmes + 1;
          vnteste_diafixo := 0;
          GOTO TESTE_DATA;
        END IF;

      END LOOP;

    END LOOP;

  END CARREGAR_DTVENCIMENTO;
  --------
  -- CARREGAR_DTVENCIMENTO  - 5 parametros
  PROCEDURE CARREGAR_DTVENCIMENTO (pddtemissao  IN DATE,
                                   pncarencia   IN NUMBER,
                                   pndiafixo    IN NUMBER,
                                   pnumparcelas IN NUMBER,
                                   pncodcli     IN NUMBER)
  IS
    vnteste_diafixo         NUMBER(1);
    vddata_vencimento       DATE;
    vndiafixo               NUMBER(2);
    vnmes                   NUMBER(2);
    vb_diavalido            BOOLEAN;
    vnverifmesatual         NUMBER(3);

    vndiasfaltantes         NUMBER(2);
    vndiaVencimento         NUMBER(2);
    vnDiferencaDia          NUMBER(2);
    vndiafixocliente        NUMBER(2);
  BEGIN
    vb_diavalido    := FALSE;
    vnmes           := 0;
    vnverifmesatual := 0;
    G_DATAS.DELETE;

    vndiasfaltantes := 0;
    vndiaVencimento := 0;
    vnDiferencaDia  := 0;
    vndiafixocliente:= 0;

     FOR N IN 1..pnumparcelas
    LOOP

      G_DATAS.EXTEND;
      G_DATAS(N).PREST := TO_CHAR(N);
      vnteste_diafixo := 0;
      vnmes := vnmes + 1;
      vb_diavalido := FALSE;
      vndiafixo       := pndiafixo;
      WHILE (NOT vb_diavalido) LOOP
        <<TESTE_DATA>>
        WHILE vnteste_diafixo = 0 LOOP
          BEGIN
            IF (N = 1) AND (vnverifmesatual = 0) THEN --Se for a primeira parcela, verifica se o dia atual é menor que o dia fixo
              IF (EXTRACT(DAY FROM SYSDATE+pncarencia)) < vndiafixo THEN
                 vnmes := 0;
                 vnverifmesatual := vnverifmesatual + 1;
              END IF;
            END IF;

            vddata_vencimento := TRUNC(SYSDATE)+pncarencia;

            if(vndiafixo > 0)then
             SELECT TO_DATE(TO_CHAR(vndiafixo) || '/' ||
                           TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE) + pncarencia, 0),
                                   'MM/YYYY'),
                           'DD/MM/YYYY')
              INTO vddata_vencimento
              FROM DUAL;


            end if;
            vnteste_diafixo := 1;
          EXCEPTION
            WHEN OTHERS THEN
              vndiafixo := vndiafixo - 1;
          END;
        END LOOP;

        IF ((pddtemissao + pncarencia) <= vddata_vencimento) or (vndiafixocliente = EXTRACT(DAY FROM vddata_vencimento)) THEN

          if(vndiafixocliente = 0)then
            vnDiferencaDia := 0;
            --Loop nos dias de pagamento do cliente
            for reg in (select DIA FROM PCDIASPLPAGCLI WHERE CODCLI = pncodcli order by DIA ASC) LOOP
              --Armazena dia da parcela a ser gerada.
              vndiaVencimento := EXTRACT(DAY FROM vddata_vencimento);
              --Compara o dia cadastrado com o dia a ser gerado
              if(reg.DIA <> 0)then
                --verifica se e o dia mais proximo ate o momento
                  IF (ABS(reg.DIA - vndiaVencimento)  <= ABS(vnDiferencaDia))or (vndiafixocliente = 0)then
                    vnDiferencaDia := (reg.DIA - vndiaVencimento);
                    vndiafixocliente := reg.DIA;
                  end if;
                end if;

            END LOOP;

            if(vndiafixocliente > 0)then
              vnmes := vnmes + 1;
              vnteste_diafixo := 0;
              vndiafixo := vndiafixocliente;
              IF (EXTRACT(DAY FROM SYSDATE)) < vndiafixo THEN
                vnverifmesatual := 1;
              end if;
              GOTO TESTE_DATA;
            end if;
          end if;

          G_DATAS(N).DTVENCIMENTO := vddata_vencimento;
          vb_diavalido := TRUE;
          vndiafixocliente := 0;

        ELSE
          IF(N=1)THEN--Se a primeira parcela + carencia for invalida o dia fixo volta ao original e incrementa o mes.
           if(pndiafixo = 0)then
            vndiafixo := vndiafixo +1;
           else
            vndiafixo := pndiafixo;
            vnmes := vnmes + 1;
           end if;
          END IF;
          vnteste_diafixo := 0;
          GOTO TESTE_DATA;
        END IF;

      END LOOP;

    END LOOP;

  END CARREGAR_DTVENCIMENTO;

  ------

  PROCEDURE validarInserirCobranca(pscodcob   IN pccob.codcob%TYPE,
                                   psmensagem IN OUT VARCHAR2)
  /******************************************************************************************
     ->Nome da PROCEDURE : ValidarInserirCobranca
     ->Objetivo          : Verificar se as Cobranças ('CH', 'SENT', 'BNF', 'BNFT') existem, caso não
                           será inserido em PCCOB
     ->Versao            : Pacote
     -------------------------------- Historico ------------------------------------------------------------------
     Data          Responsavel  Tarefa      Comentarios
     ------------  ------------ ----------  -----------------------------------------------------------------------------
     07/12/2007    Pablo        55249       Procedure criada para verificar se existe, caso contrário irá criar o
                                            o registro da Cobrança que é setado automáticamento na 316
     10/04/2008    Pablo        58743       Caso a cobrança 'CRED' não exista a mesma será inserida automáticamente.
    ******************************************************************************************/
   IS
    vicontador NUMBER;
  BEGIN
    vicontador := 0;

    SELECT COUNT(*) INTO vicontador FROM pccob WHERE codcob = pscodcob;

    IF vicontador = 0 THEN
      INSERT INTO pccob
        (codcob, cobranca, pagcomissao)
      VALUES
        (pscodcob,
         DECODE(pscodcob,
                'CH',
                'CHEQUE',
                'SENT',
                'COBRANCA DE ENVIO',
                'BNF',
                'BONIFICACAO',
                'BNFT',
                'BONIFICACAO DE TROCA',
                'CRED',
                'CREDITO',
                'DH',
                'DINHEIRO EM TRANSITO'),
         'N');
    END IF;

    PSMENSAGEM := 'OK';

  EXCEPTION
    WHEN OTHERS THEN
      psmensagem := 'Não foi possível gerar registro na tabela de cobrança.' ||
                    'Erro: ' || SQLCODE || '-' || SQLERRM || ' - ' ||
                    DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
  END validarinserircobranca;

  PROCEDURE P_GERARPARCELATROCO(PVALOR        IN NUMBER,
                                PPREST        IN NUMBER,
                                PCODMOTORISTA IN NUMBER,
                                ROW_PCNFSAID  IN PCNFSAID%ROWTYPE,
                                cab           IN FATURAMENTO.C_PEDIDOS%ROWTYPE,
                                vrPARAMETROS  IN PCPARAMFAT%ROWTYPE) IS
  BEGIN

    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
    FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando parcela referente ao valor de troco...';

    FATURAMENTO. GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                           (CASE WHEN
                            FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN
                            DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                            TO_CHAR($$PLSQL_LINE) END));

    INSERT INTO pcprest
      (codfilial, -- 1
       codcli, -- 2
       codusur, -- 3
       prest, -- 4
       duplic, -- 5
       valor, -- 6
       codcob, -- 7
       dtvenc, -- 8
       dtvencorig, -- 9
       dtemissao, -- 10
       numcar, -- 11
       codsupervisor, -- 12
       numtransvenda, -- 13
       valororig, -- 14
       codcoborig, --15
       codfilialnf, --16
       numped, --17
       dtcriacao, -- 18
       dtemissaoorig, -- 19
       status, --20
       numtransvendast, --21
       operacao, --22
       boleto, --24
       percom, --25
       percom2, --26
       percom3, --27
       percom4, --28
       valorliqcom, --29
       vltxboleto, --30
       numcheckout, --31
       codemitentepedido, --32
       somatxboleto, --33
       presttef, --34
       CODFUNCCHECKOUT, --35
       codusur2, --36
       codusur3, --37
       codusur4, --38
       codmotorista --39
       )
    VALUES
      (cab.codfilial, --1
       cab.codcli, --2
       cab.codusur, --3
       PPREST, --4
       ROW_PCNFSAID.NUMNOTA, --oNFSAIDA.NUMNOTA, --5
       PVALOR * (-1), --6
       'TR', --7
       trunc(SYSDATE), --8
       trunc(SYSDATE), --9
       TRUNC(SYSDATE), --10
       cab.numcar, --11
       cab.codsupervisor, --12
       ROW_PCNFSAID.NUMTRANSVENDA, --VN_NUMTRANSVENDA, --13
       PVALOR, --14
       'TR', --15
       cab.codfilialnf, --16
       cab.numped, --17
       TRUNC(SYSDATE), --18
       TRUNC(SYSDATE), --19
       'A', --20,
       NULL, --21
       'N', --22
       '1', --24
       0, --25
       0, --26
       0, --27
       0, --28
       PVALOR *(-1), --29
       0, --30
       FATURAMENTO.G_FUNC.NUMCAIXABALCAO, --31
       cab.codemitente, --32
       vrPARAMETROS.CON_SOMATXBOLETO, --33
       PPREST, --34
       DECODE(FATURAMENTO.G_FUNC.NUMCAIXABALCAO,
              0,
              NULL,
              FATURAMENTO.G_FUNC.MATRICULA), --35
       cab.codusur2, --36
       cab.codusur3, --37
       cab.codusur4, --38
       PCODMOTORISTA);

  END P_GERARPARCELATROCO;

  PROCEDURE P_GERARPARCELAENTRADA(PVALOR        IN NUMBER,
                                  PPREST        IN NUMBER,
                                  PCODMOTORISTA IN NUMBER DEFAULT 0,
                                  PPERCOM       IN NUMBER,
                                  PPERCOM2      IN NUMBER,
                                  PPERCOM3      IN NUMBER,
                                  PPERCOM4      IN NUMBER,
                                  PPERCOMLIQ    IN NUMBER,
                                  ROW_PCNFSAID  IN PCNFSAID%ROWTYPE,
                                  cab           IN FATURAMENTO.C_PEDIDOS%ROWTYPE,
                                  vrPARAMETROS  IN PCPARAMFAT%ROWTYPE) IS
  BEGIN

    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
    FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando parcela do valor de entrada...';

    FATURAMENTO. GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                           (CASE WHEN
                            FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN
                            DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                            TO_CHAR($$PLSQL_LINE) END));

    INSERT INTO pcprest
      (codfilial, -- 1
       codcli, -- 2
       codusur, -- 3
       prest, -- 4
       duplic, -- 5
       valor, -- 6
       codcob, -- 7
       dtvenc, -- 8
       dtvencorig, -- 9
       dtemissao, -- 10
       numcar, -- 11
       codsupervisor, -- 12
       numtransvenda, -- 13
       valororig, -- 14
       codcoborig, --15
       codfilialnf, --16
       numped, --17
       dtcriacao, -- 18
       dtemissaoorig, -- 19
       status, --20
       numtransvendast, --21
       operacao, --22
       boleto, --24
       percom, --25
       percom2, --26
       percom3, --27
       percom4, --28
       valorliqcom, --29
       vltxboleto, --30
       numcheckout, --31
       codemitentepedido, --32
       somatxboleto, --33
       presttef, --34
       CODFUNCCHECKOUT, --35
       codusur2, --36
       codusur3, --37
       codusur4, --38
       codmotorista, --39
       percomliq, --40
       codcobsefaz --41
       )
    VALUES
      (cab.codfilial, --1
       cab.codcli, --2
       cab.codusur, --3
       PPREST, --4
       ROW_PCNFSAID.NUMNOTA, --oNFSAIDA.NUMNOTA, --5
       PVALOR, --6
       'DH', --7
       trunc(SYSDATE), --8
       trunc(SYSDATE), --9
       TRUNC(SYSDATE), --10
       cab.numcar, --11
       cab.codsupervisor, --12
       ROW_PCNFSAID.NUMTRANSVENDA, --VN_NUMTRANSVENDA, --13
       PVALOR, --14
       'DH', --15
       cab.codfilialnf, --16
       cab.numped, --17
       TRUNC(SYSDATE), --18
       TRUNC(SYSDATE), --19
       'A', --20,
       NULL, --21
       'N', --22
       '1', --24
       PPERCOM, --25
       PPERCOM2, --26
       PPERCOM3, --27
       PPERCOM4, --28
       PVALOR, --29
       0, --30
       FATURAMENTO.G_FUNC.NUMCAIXABALCAO, --31
       cab.codemitente, --32
       vrPARAMETROS.CON_SOMATXBOLETO, --33
       PPREST, --34
       DECODE(FATURAMENTO.G_FUNC.NUMCAIXABALCAO,
              0,
              NULL,
              FATURAMENTO.G_FUNC.MATRICULA), --35
       cab.codusur2, --36
       cab.codusur3, --37
       cab.codusur4, --38
       PCODMOTORISTA,--39
       PPERCOMLIQ,--40
       (select CODCOBSEFAZ from pccob where codcob = 'DH')--41
       );

       --VEN-813
       FATURAMENTO.vrPARAMETROS.MSG := 'Gerando parcela do valor de entrada codcob DH e codcoborig DH ...';
       FATURAMENTO. GRAVARLOG(FATURAMENTO.vrPARAMETROS, TO_CHAR($$PLSQL_LINE));

  END P_GERARPARCELAENTRADA;

  PROCEDURE P_GERARPARCELA_BNF(PVALOR        IN NUMBER,
                               PPREST        IN NUMBER,
                               PCODMOTORISTA IN NUMBER DEFAULT 0,
                               ROW_PCNFSAID  IN PCNFSAID%ROWTYPE,
                               cab           IN FATURAMENTO.C_PEDIDOS%ROWTYPE,
                               vrPARAMETROS  IN PCPARAMFAT%ROWTYPE) IS
    vsGerarBnfFechada varchar2(1);
    VSCODCOBSEFAZAUX  PCCOB.CODCOBSEFAZ%TYPE;
  BEGIN

    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
    FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando parcela do valor de bonificação...';

    FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS);

    IF (vrPARAMETROS.BAIXARBONFICFATURAMENTO = 'S') THEN
      vsGerarBnfFechada := 'S';
    ELSE
      vsGerarBnfFechada := 'N';
    END IF;

    SELECT  CODCOBSEFAZ
      INTO VSCODCOBSEFAZAUX
      FROM pccob
     WHERE pccob.codcob = 'BNF';

    INSERT INTO pcprest
      (codfilial, -- 1
       codcli, -- 2
       codusur, -- 3
       prest, -- 4
       duplic, -- 5
       valor, -- 6
       codcob, -- 7
       dtvenc, -- 8
       dtvencorig, -- 9
       dtemissao, -- 10
       numcar, -- 11
       codsupervisor, -- 12
       numtransvenda, -- 13
       valororig, -- 14
       codcoborig, --15
       codfilialnf, --16
       numped, --17
       dtcriacao, -- 18
       dtemissaoorig, -- 19
       status, --20
       numtransvendast, --21
       operacao, --22
       boleto, --24
       percom, --25
       percom2, --26
       percom3, --27
       percom4, --28
       valorliqcom, --29
       vltxboleto, --30
       numcheckout, --31
       codemitentepedido, --32
       somatxboleto, --33
       presttef, --34
       CODFUNCCHECKOUT, --35
       codusur2, --36
       codusur3, --37
       codusur4, --38
       codmotorista, --39
       VPAGO, -- 40
       DTPAG, -- 41
       DTFECHA, -- 42
       DTBAIXA, -- 43
       CODBAIXA, -- 44
       CODFUNCFECHA, -- 45
       HORAFECHA, -- 46
       MINUTOFECHA, -- 47
       DTCXMOT, -- 48
       CODCOBSEFAZ --49
       )
    VALUES
      (cab.codfilial, --1
       cab.codcli, --2
       cab.codusur, --3
       PPREST, --4
       ROW_PCNFSAID.NUMNOTA, --oNFSAIDA.NUMNOTA, --5
       PVALOR, --6
       'BNF', --7
       trunc(SYSDATE), --8
       trunc(SYSDATE), --9
       TRUNC(SYSDATE), --10
       cab.numcar, --11
       cab.codsupervisor, --12
       ROW_PCNFSAID.NUMTRANSVENDA, --VN_NUMTRANSVENDA, --13
       PVALOR, --14
       'BNF', --15
       cab.codfilialnf, --16
       cab.numped, --17
       TRUNC(SYSDATE), --18
       TRUNC(SYSDATE), --19
       'A', --20,
       NULL, --21
       'N', --22
       '1', --24
       0, --25
       0, --26
       0, --27
       0, --28
       0, --29
       0, --30
       FATURAMENTO.G_FUNC.NUMCAIXABALCAO, --31
       cab.codemitente, --32
       vrPARAMETROS.CON_SOMATXBOLETO, --33
       PPREST, --34
       DECODE(FATURAMENTO.G_FUNC.NUMCAIXABALCAO,
              0,
              NULL,
              FATURAMENTO.G_FUNC.MATRICULA), --35
       cab.codusur2, --36
       cab.codusur3, --37
       cab.codusur4, --38
       PCODMOTORISTA, --39
       DECODE(vsGerarBnfFechada, 'S', PVALOR, NULL), --40
       DECODE(vsGerarBnfFechada, 'S', trunc(sysdate), NULL), --41
       DECODE(vsGerarBnfFechada, 'S', trunc(sysdate), NULL), --42
       DECODE(vsGerarBnfFechada, 'S', trunc(sysdate), NULL), --43
       DECODE(vsGerarBnfFechada, 'S', FATURAMENTO.G_FUNC.MATRICULA, NULL), --44
       DECODE(vsGerarBnfFechada, 'S', FATURAMENTO.G_FUNC.MATRICULA, NULL), --45
       DECODE(vsGerarBnfFechada, 'S', TO_CHAR(SYSDATE, 'hh24'), NULL), --46
       DECODE(vsGerarBnfFechada, 'S', TO_CHAR(SYSDATE, 'mi'), NULL), --47
       DECODE(vsGerarBnfFechada, 'S', trunc(sysdate), NULL), --48
       VSCODCOBSEFAZAUX --49
       );

  END P_GERARPARCELA_BNF;

  FUNCTION F_RETORNAR_COB_ACRES_DIFER(PNUMPED IN NUMBER) RETURN VARCHAR2 IS
    VSCODCOBDIFER  PCFORMAPGTOPEDIDO.CODCOB%TYPE;

    FUNCTION F_CODCOB_BOLETO(PCODCOB IN VARCHAR2) RETURN BOOLEAN IS
      I NUMBER;
    BEGIN
      BEGIN
        SELECT COUNT(1)
          INTO I
          FROM PCCOB
         WHERE CODCOB = PCODCOB
           AND NVL(BOLETO, 'N') = 'S';
      EXCEPTION
        WHEN OTHERS THEN
          I := 0;
      END;


      RETURN I > 0;
    END;
  BEGIN
    VSCODCOBDIFER := FERRAMENTAS.F_BUSCARPARAMETRO_ALFA('CODCOBDIFERFINANFATUR', '99', 'D');

    FOR COB IN (SELECT CODCOB
                  FROM PCFORMAPGTOPEDIDO
                 WHERE NUMPED = PNUMPED)
    LOOP
    -- cheque : ch, chy
    -- dinheiro: d, dh
    --boleto: pccob.BOLETO = 'S';
      IF (VSCODCOBDIFER = 'D') AND
         (COB.CODCOB IN ('D','DH'))
      THEN
        RETURN COB.CODCOB;
      END IF;

      IF (VSCODCOBDIFER = 'C') AND
         (COB.CODCOB IN ('CH','CHT'))
      THEN
        RETURN COB.CODCOB;
      END IF;

      IF (VSCODCOBDIFER = 'B') AND
         (F_CODCOB_BOLETO(COB.CODCOB) = TRUE)
      THEN
        RETURN COB.CODCOB;
      END IF;

    END LOOP;


    RETURN '';
  END F_RETORNAR_COB_ACRES_DIFER;

  PROCEDURE buscarDadosAuxiliares(PNNUMPED          IN      PCPEDC.NUMPED%TYPE,
                                  VSBOLETO          IN  OUT PCCOB.BOLETO%TYPE,
                                  VNCODSUPERVISOR   IN  OUT PCUSUARI.CODSUPERVISOR%TYPE,
                                  VNNUMREGIAO       IN  OUT PCPRACA.NUMREGIAO%TYPE,
                                  PSMSG                 OUT VARCHAR2,
                                  P_FORMAPGTOPEDIDO IN      PCFORMAPGTOPEDIDO%ROWTYPE DEFAULT NULL)
  /******************************************************************************************
     -> PROGRAMA UTILIZADO PELA PROCEDURE DE FATURAMENTO
     -> VERSAO : PACOTE
    *************************************************************************************************************
     07/03/2006    SABRINA TAREFA 25188: TOO LARGE - INCLUSAO DE VARIVAL ABATERIMPOSTOSCOMISSAORCA
     14/06/2006    PABLO: ACRESCENTADO VALIDAÇÃO PARA VERIFICAR SE PCTABPRCLI.NUMREGIAO IS NULL
     13/06/2017    GUILHERME FREITAS: AJUSTADO PARA VALIDAR A COBRANÇA DA COBRANÇA QUANDO UTILIZANDO
                   MULTIPLOS PLANOS DE PAGAMENTO
    *************************************************************************************************************/
   IS
    VICONTADOR    NUMBER;
    VSCODCOB      PCPEDC.CODCOB%TYPE;
    VNCODUSUR     PCPEDC.CODUSUR%TYPE;
    VNCODPRACA    PCPEDC.CODPRACA%TYPE;
    VNCODCLI      PCPEDC.CODCLI%TYPE;
    VSCODFILIALNF PCPEDC.CODFILIALNF%TYPE;
    ERRO_DADOS EXCEPTION;
  BEGIN
    PSMSG := 'OK';

    BEGIN
      SELECT NVL(P_FORMAPGTOPEDIDO.CODCOB, CODCOB), CODUSUR, CODCLI, NVL(CODFILIALNF, CODFILIAL)
        INTO VSCODCOB, VNCODUSUR, VNCODCLI, VSCODFILIALNF
        FROM PCPEDC
       WHERE NUMPED = PNNUMPED;

      SELECT CODPRACA
        INTO VNCODPRACA
        FROM PCCLIENT
       WHERE CODCLI = VNCODCLI;

      BEGIN
        SELECT NVL(BOLETO, 'N')
          INTO VSBOLETO
          FROM PCCOB
         WHERE CODCOB = VSCODCOB;
      EXCEPTION
        WHEN OTHERS THEN
          PSMSG := SQLCODE || '-' || SQLERRM || ' SELECAO DA COBRANCA. ' ||
                   ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
      END;

      BEGIN
        --RECUPERA OS DADOS DO RCA
        SELECT CODSUPERVISOR
          INTO VNCODSUPERVISOR
          FROM PCUSUARI
         WHERE CODUSUR = VNCODUSUR;
      EXCEPTION
        WHEN OTHERS THEN
          PSMSG := SQLCODE || '-' || SQLERRM || ' SELECAO DO VENDEDOR. ' ||
                   ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
      END;

      BEGIN
        --RECUPERA O NUM. DA REGIAO
        SELECT NUMREGIAO
          INTO VNNUMREGIAO
          FROM PCPRACA
         WHERE CODPRACA = VNCODPRACA;
      EXCEPTION
        WHEN OTHERS THEN
          PSMSG := SQLCODE || '-' || SQLERRM || ' SELECAO DA REGIAO. ' ||
                   ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
      END;

      BEGIN
        -- VERIFICAR A EXISTENCIA DE TABELAS DE PRECOS POR CLIENTE
        SELECT COUNT(*)
          INTO VICONTADOR
          FROM PCTABPRCLI
         WHERE CODCLI = VNCODCLI
           AND CODFILIALNF = VSCODFILIALNF;

        IF VICONTADOR = 1 THEN
          SELECT NUMREGIAO
            INTO VNNUMREGIAO
            FROM PCTABPRCLI
           WHERE CODCLI = VNCODCLI
             AND CODFILIALNF = VSCODFILIALNF;

          IF VNNUMREGIAO IS NULL THEN
            RAISE ERRO_DADOS;
          END IF;
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          PSMSG := SQLCODE || '-' || SQLERRM ||
                   ' BUSCARDADOSAUXILIARES - SELECAO DE TABELAS POR CLIENTE. ' ||
                   ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
      END;
    EXCEPTION
      WHEN ERRO_DADOS THEN
        PSMSG := ' REGIÃO DO PCTABPRCLI NÃO INFORMADA PARA CLIENTE ' ||
                 TO_CHAR(VNCODCLI) || ' FILIAL NF' || VSCODFILIALNF;
      WHEN OTHERS THEN
        PSMSG := SQLCODE || '-' || SQLERRM ||
                 ' SELECAO DE DADOS DO PEDIDO. ' || ' - ' ||
                 DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
        RAISE;
    END;
  END BUSCARDADOSAUXILIARES;

  /* PROCEDURE INICIARVARIAVEIS() IS
    BEGIN

            vnvltarifa              := 0;
            vntotalger              := 0;
            vntotalger_ant          := 0;
            vncomissao              := 0;
            vncomissao2             := 0;
            vncomissao3             := 0;
            vncomissao4             := 0;
            vnicmsretido            := 0;
            vnicmsretidognre        := 0;
            vntotalipi              := 0;
            vntotalvldifaliquotas   := 0;
            vbdescboletoparc1       := FALSE;
            vnvlentrada             := 0;
            vntotvldesconto         := 0;
            vipresttef_desc         := 0;

    END;
  */
--P_PRODUTOS IN TABLE_NUMBER,  ¿Recebe as chaves de produtos
--O_CURSOR out sys_refcursor


  PROCEDURE retornarPERCOM(pnNUMTRANSVENDA   IN NUMBER,
                          vrPARAMETROS      IN PCPARAMFAT%ROWTYPE,
                          VNNUMPARCCOB      in number,
                          PSCODCOB          in VARCHAR2 := 'N',
                          PSTIPOPARCELA     IN VARCHAR2,
                          OUTPERCOM         OUT NUMBER,
                          OUTPERCOM2        OUT NUMBER,
                          OUTPERCOM3        OUT NUMBER,
                          OUTPERCOM4        OUT NUMBER,
                          OUTPERCOMLIQ      OUT NUMBER) IS
    VNTOTALGERPERCOMLIQ NUMBER;
    VNTOTALGERPERCOM    NUMBER;
    VNJUROS             number := 0;
    VNCOMISSAO          number;
    VNRETORNOPERCOM     number;

    TYPE T_Dados_PERCOM IS RECORD(
      VLTOTGER                     pcnfsaid.VLTOTGER%type,
      VLIPI                        pcnfsaid.VLIPI%type,
      ICMSRETIDO                   pcnfsaid.ICMSRETIDO%type,
      Comissao                     pcnfsaid.Comissao%type,
      Comissao2                    pcnfsaid.Comissao2%type,
      Comissao3                    pcnfsaid.Comissao3%type,
      Comissao4                    pcnfsaid.Comissao4%type,
      VSUTILIZAPROCESSOGERACAOCPST PCCLIENT.GERATITULOST%type,
      vldescfin                    pcnfsaid.vldescfin%type,
      CODCOB                       pcnfsaid.CODCOB%type

      );

    G_dadosPercom T_Dados_PERCOM;
    PROCEDURE carregarDadosIniciais(pnNUMTRANSVENDA IN PCNFSAID.NUMTRANSVENDA%TYPE) IS
    begin
      SELECT PCNFSAID.VLTOTGER,
             PCNFSAID.VLIPI,
             PCNFSAID.ICMSRETIDO,
             PCNFSAID.Comissao,
             PCNFSAID.Comissao2,
             PCNFSAID.Comissao3,
             PCNFSAID.Comissao4,
             NVL(PCCLIENT.GERATITULOST, 'N') VSUTILIZAPROCESSOGERACAOCPST,
             pcnfsaid.vldescfin,
             PCNFSAID.CODCOB
        INTO G_dadosPercom
        FROM PCNFSAID, PCPLPAG, PCCOB, PCCLIENT, PCPEDC
       WHERE PCNFSAID.CODPLPAG = PCPLPAG.CODPLPAG
         AND PCNFSAID.NUMTRANSVENDA = PNNUMTRANSVENDA
         AND PCNFSAID.CODCOB = PCCOB.CODCOB
         AND PCNFSAID.CODCLI = PCCLIENT.CODCLI
         AND PCNFSAID.NUMPED = PCPEDC.NUMPED;
    end;

    FUNCTION calcularPercom(pncampo in varchar2, pnvltotger in number)
      return number is
      VSPAGCOMISSAO VARCHAR2(1);
    begin
      BEGIN
       /* VSPAGCOMISSAO := VSPAGCOMISSAO_COB;
        IF VSPAGCOMISSAO IS NULL THEN
          SELECT NVL(PAGCOMISSAO, 'N')
            INTO VSPAGCOMISSAO
            FROM PCCOB
           WHERE CODCOB = G_dadosPercom.CODCOB;

        END IF;*/

  --      IF VNCODCOBMULT IS NOT NULL THEN
          SELECT NVL(PAGCOMISSAO, 'N')
            INTO VSPAGCOMISSAO
            FROM PCCOB
           WHERE CODCOB = PSCODCOB;
    --    END IF;
      EXCEPTION
        WHEN OTHERS THEN
          VSPAGCOMISSAO := 'N';
      END;

      if pncampo = 'PERCOM' then
        VNCOMISSAO := G_dadosPercom.Comissao;
      elsif pncampo = 'PERCOM2' then
        VNCOMISSAO := G_dadosPercom.Comissao2;
      elsif pncampo = 'PERCOM3' then
        VNCOMISSAO := G_dadosPercom.Comissao3;
      elsif pncampo = 'PERCOM4' then
        VNCOMISSAO := G_dadosPercom.Comissao4;
      else
        VNCOMISSAO := G_dadosPercom.Comissao;
      end if;

      VNRETORNOPERCOM := CASE VSPAGCOMISSAO WHEN 'N' THEN 0 ELSE CASE WHEN(NVL(pnvltotger, 0) = 0) THEN 0 ELSE CASE vrparametros.ABATERDESCFINCOMISSAORCA WHEN 'S' THEN ROUND(((VNCOMISSAO / (((pnvltotger --- NVL(G_dadosPercom.vldescfin, 0)
       ) - (NVL(VNJUROS, 0) * VNNUMPARCCOB)))) * 100), 5) --- 207547
       WHEN 'N' THEN ROUND(((VNCOMISSAO / ((pnvltotger - (NVL(VNJUROS, 0) * VNNUMPARCCOB)))) * 100), 5) END END END;
      RETURN VNRETORNOPERCOM;
    end;

  BEGIN
    carregarDadosIniciais(pnNUMTRANSVENDA);

    VNTOTALGERPERCOM    := G_dadosPercom.VLTOTGER; --VNTOTALGER;
    VNTOTALGERPERCOMLIQ := G_dadosPercom.VLTOTGER; --VNTOTALGER;

    IF PSTIPOPARCELA = 'COMUM' THEN
      if NVL(G_dadosPercom.VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'S' then
        -- gera parcele ST
        CASE vrPARAMETROS.IMPOSTOCOMISSAORCA
          WHEN 'A' THEN
            VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ - G_dadosPercom.VLIPI -
                                   G_dadosPercom.ICMSRETIDO;
            VNTOTALGERPERCOM    := VNTOTALGERPERCOM -
                                   G_dadosPercom.ICMSRETIDO;
          WHEN 'I' THEN
            VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ - G_dadosPercom.VLIPI;
            --VNTOTALGERPERCOM    := VNTOTALGERPERCOM - G_dadosPercom.VLIPI;
          WHEN 'S' THEN
            VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ -
                                   G_dadosPercom.ICMSRETIDO;
            VNTOTALGERPERCOM    := VNTOTALGERPERCOM -
                                   G_dadosPercom.ICMSRETIDO;
        END CASE;ELSE
        CASE vrPARAMETROS.IMPOSTOCOMISSAORCA
          WHEN 'A' THEN
            VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ -
                                   G_dadosPercom.VLIPI -
                                   G_dadosPercom.ICMSRETIDO;
            --          VNTOTALGERPERCOM    := VNTOTALGERPERCOM - G_dadosPercom.ICMSRETIDO;
          WHEN 'I' THEN
            VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ -
                                   G_dadosPercom.VLIPI;
            --VNTOTALGERPERCOM    := VNTOTALGERPERCOM - G_dadosPercom.VLIPI;
          WHEN 'S' THEN
            VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ -
                                   G_dadosPercom.ICMSRETIDO;
            --          VNTOTALGERPERCOM    := VNTOTALGERPERCOM - G_dadosPercom.ICMSRETIDO;
        END CASE; END IF;
    ELSIF PSTIPOPARCELA = 'ST' THEN
      CASE vrPARAMETROS.IMPOSTOCOMISSAORCA
        WHEN 'A' THEN
          VNTOTALGERPERCOMLIQ := 0; --VNTOTALGERPERCOM - (vntotalipi) - (VNVALORPARCST);
          VNTOTALGERPERCOM    := 0;
        WHEN 'I' THEN
          VNTOTALGERPERCOMLIQ := VNTOTALGERPERCOMLIQ - G_dadosPercom.VLIPI;
        WHEN 'S' THEN
          VNTOTALGERPERCOMLIQ := 0; --VNTOTALGERPERCOM - (VNVALORPARCST);
          VNTOTALGERPERCOM    := 0;
      END CASE; END IF;

    OUTPERCOMLIQ := calcularPercom('PERCOMLIQ', VNTOTALGERPERCOMLIQ);
    OUTPERCOM    := calcularPercom('PERCOM', VNTOTALGERPERCOM);
    OUTPERCOM2   := calcularPercom('PERCOM2', VNTOTALGERPERCOM);
    OUTPERCOM3   := calcularPercom('PERCOM3', VNTOTALGERPERCOM);
    OUTPERCOM4   := calcularPercom('PERCOM4', VNTOTALGERPERCOM);
  end retornarPERCOM;

  function gerarFinanceiroAutosservico(ROW_PCNFSAID IN PCNFSAID%ROWTYPE,
                                       P_CAB        IN FATURAMENTO.C_PEDIDOS%ROWTYPE,
                                       vrPARAMETROS IN PCPARAMFAT%ROWTYPE)
    return boolean is
    viContPrest   integer;
    ROWPCPREST    PCPREST%ROWTYPE;
    pvc2menssagen varchar2(4000);
    VBPULOUPEDIDO boolean;
  begin

    VBPULOUPEDIDO := false;

    begin
      VICONTPREST := 0;
      FOR PREST IN (SELECT *
                      FROM PCPRESTECF
                     WHERE PCPRESTECF.NUMPEDECF = P_CAB.NUMPEDECF --PCPEDC.NUMPEDECF
                       AND PCPRESTECF.NUMCHECKOUT = P_CAB.NUMCAIXA -- PCPEDC.NUMCAIXA
                       AND PCPRESTECF.CODFUNCCHECKOUT = P_CAB.CODFUNCCX) LOOP

        VICONTPREST := VICONTPREST + 1;

        ROWPCPREST.DUPLIC    := ROW_PCNFSAID.NUMNOTA; --PREST.DUPLIC;
        ROWPCPREST.NUMPED    := ROW_PCNFSAID.NUMPED; --PREST.DUPLIC;
        ROWPCPREST.PREST     := VICONTPREST;
        ROWPCPREST.CODFILIAL := PREST.CODFILIAL;
        ROWPCPREST.CODCLI    := PREST.CODCLI;
        ROWPCPREST.CODUSUR   := PREST.CODUSUR;

        ROWPCPREST.VALOR  := PREST.VALOR;
        ROWPCPREST.CODCOB := PREST.CODCOB;

        ROWPCPREST.DTVENC     := PREST.DTVENC;
        ROWPCPREST.DTVENCORIG := PREST.DTVENCORIG;
        ROWPCPREST.DTEMISSAO  := PREST.DTEMISSAO;
        ROWPCPREST.OPERACAO   := PREST.OPERACAO;
        ROWPCPREST.BOLETO     := PREST.BOLETO;
        ROWPCPREST.STATUS     := PREST.STATUS;

        ROWPCPREST.NUMCAR        := PREST.NUMCAR;
        ROWPCPREST.CODSUPERVISOR := PREST.CODSUPERVISOR;
        ROWPCPREST.NUMTRANSVENDA := ROW_PCNFSAID.NUMTRANSVENDA; --PREST.NUMTRANSVENDA;
        ROWPCPREST.VLTXBOLETO    := PREST.VLTXBOLETO;
        ROWPCPREST.VALORORIG     := PREST.VALORORIG;
        ROWPCPREST.CODCOBORIG    := PREST.CODCOBORIG;

        ROWPCPREST.CODFILIALNF     := PREST.CODFILIALNF;
        ROWPCPREST.NUMCHECKOUT     := PREST.NUMCHECKOUT;
        ROWPCPREST.CODFUNCCHECKOUT := PREST.CODFUNCCHECKOUT;
        ROWPCPREST.CODBARRA        := PREST.CODBARRA;
        ROWPCPREST.NUMBANCO        := PREST.NUMBANCO;

        ROWPCPREST.NUMAGENCIA       := PREST.NUMAGENCIA;
        ROWPCPREST.NUMCHEQUE        := PREST.NUMCHEQUE;
        ROWPCPREST.NUMCONTACORRENTE := PREST.NUMCONTACORRENTE;

        ROWPCPREST.NSUTEF           := PREST.NSUTEF;
        ROWPCPREST.NUMCONTACORRENTE := PREST.NSUHOST;
        ROWPCPREST.PRESTTEF         := PREST.PRESTTEF;

        IF (PREST.CODCOB = 'CRED') THEN

          ROWPCPREST.VPAGO         := PREST.VALOR;
          ROWPCPREST.DTPAG         := SYSDATE;
          ROWPCPREST.DTULTALTER    := SYSDATE;
          ROWPCPREST.DTBAIXA       := SYSDATE;
          ROWPCPREST.DTFECHA       := TRUNC(SYSDATE);
          ROWPCPREST.DTMOVIMENTOCX := TRUNC(SYSDATE);
          ROWPCPREST.DTCXMOT       := SYSDATE;
          ROWPCPREST.rotinalanc    := 1400;
          ROWPCPREST.CODBAIXA      := PREST.CODFUNCCHECKOUT;
          ROWPCPREST.CODFUNCCXMOT  := PREST.CODFUNCCHECKOUT;
          ROWPCPREST.CODFUNCFECHA  := PREST.CODFUNCCHECKOUT;
          ROWPCPREST.DTEMISSAOORIG := PREST.DTEMISSAO;
        ELSE
          ROWPCPREST.VPAGO         := NULL;
          ROWPCPREST.DTPAG         := NULL;
          ROWPCPREST.DTULTALTER    := NULL;
          ROWPCPREST.DTBAIXA       := NULL;
          ROWPCPREST.DTFECHA       := NULL;
          ROWPCPREST.DTMOVIMENTOCX := NULL;
          ROWPCPREST.DTCXMOT       := NULL;
          ROWPCPREST.rotinalanc    := NULL;
          ROWPCPREST.CODBAIXA      := NULL;
          ROWPCPREST.CODFUNCCXMOT  := NULL;
          ROWPCPREST.CODFUNCFECHA  := NULL;
          ROWPCPREST.DTEMISSAOORIG := NULL;

        END IF;
        INSERT INTO PCPREST VALUES ROWPCPREST;

        --JANDAIA Ajustar PCCRECLI
        UPDATE pccrecli
           SET PCCRECLI.numtransvendadesc = ROW_PCNFSAID.NUMTRANSVENDA,
               PCCRECLI.numnotadesc       = ROW_PCNFSAID.NUMNOTA
         WHERE PCCRECLI.numpedecf = P_CAB.NUMPEDECF;

      END LOOP;
    EXCEPTION
      WHEN OTHERS THEN
        pvc2menssagen                   := SQLCODE || '-' || SQLERRM ||
                                           CHR(13) ||
                                           'Erro na gracação da PCPRESTECF . AUTOSERVIÇO' ||
                                           CHR(13) ||
                                           DBMS_UTILITY.format_error_backtrace || -- Deyvid Costa - Retorna a linha que gerou a exceção
                                           CHR(13);
        FATURAMENTO.vrPARAMETROS.NUMPED := P_CAB.numped;
        FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
        FATURAMENTO.vrPARAMETROS.MSG    := pvc2menssagen;

        FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                              (CASE WHEN
                               FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN
                               DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                               TO_CHAR($$PLSQL_LINE) END));
        VBPULOUPEDIDO := TRUE;
    END;

    return VBPULOUPEDIDO;

  end;

  FUNCTION gerarFinanceiro(PN_NUMTRANSVENDA   IN PCNFSAID.NUMTRANSVENDA%TYPE, --ROW_PCNFSAID       IN PCNFSAID%ROWTYPE,
                           P_CAB                IN FATURAMENTO.C_PEDIDOS%ROWTYPE,
                           vrPARAMETROS       IN PCPARAMFAT%ROWTYPE,
                           P_FORMAPGTOPEDIDO IN PCFORMAPGTOPEDIDO%ROWTYPE DEFAULT NULL ,
                           pvc2emitente       IN VARCHAR2,
                           vsRateio_NoFinanceiro IN VARCHAR2 DEFAULT 'N' )
    RETURN T_GERARFINANCEIRO_DT IS
    --Dados de saída
    R_DADOS T_GERARFINANCEIRO_DT; --:= T_GERARFINANCEIRO_DT(NULL, NULL); --  TR_IMPOSTOS := TR_IMPOSTOS(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
    ERRO_GERAL EXCEPTION;
    ERRO_CONTASARECEBER EXCEPTION;
    ERRO_VALIDARDATAVENCIMENTO EXCEPTION;
    ROW_PCNFSAID PCNFSAID%ROWTYPE;

    PVNCODIGOCOBRANCA PCNFSAID.CODCOB%TYPE;
    vnMAXDESC NUMBER;
    vnCODCAMP NUMBER;

    -- DATE
    vddatavencimento    DATE;
    vddatavencaux       DATE;
    vddatavenc_anterior DATE;
    vddtvenc1           DATE;
    vddtvenc2           DATE;
    vddtvenc3           DATE;
    vddtvenc4           DATE;
    vddtvenc5           DATE;
    vddtvenc6           DATE;
    vddtvenc7           DATE;
    vddtvenc8           DATE;
    vddtvenc9           DATE;
    vddtvenc10          DATE;
    vddtvenc11          DATE;
    vddtvenc12          DATE;
    vddtvenccpst        DATE;
    --vddtfechavar        DATE;

    vnnumparc         NUMBER := 1;
    vnnumparccob      NUMBER := 1;
    vnnumparcimpostos NUMBER := 1;
    vnnumparcfrete    NUMBER := 1;
    vnjuros           NUMBER := 0;
    vnjurosprest      NUMBER := 0;

    vnnumdiascarencia NUMBER;

    viprazovencst_client pcclient.prazovencst%TYPE;
    -- pcplpag
    vsformaparcelamento_plpag pcplpag.formaparcelamento%TYPE;
    --Tarefa 124971
    -- PCPlPagParcelas
    vnnumdias_plpagparcelas pcplpagparcelas.numdias%TYPE;

    vnprazo1        NUMBER;
    vnprazo2        NUMBER;
    vnprazo3        NUMBER;
    vnprazo4        NUMBER;
    vnprazo5        NUMBER;
    vnprazo6        NUMBER;
    vnprazo7        NUMBER;
    vnprazo8        NUMBER;
    vnprazo9        NUMBER;
    vnprazo10       NUMBER;
    vnprazo11       NUMBER;
    vnprazo12       NUMBER;
    vntotalger      NUMBER := 0;
    vnvloutrasdesp  NUMBER;
    vnvalorparc     NUMBER;
    vnvalorparc1    NUMBER := 0;
    pvc2menssagen   VARCHAR2(255);
    vnvltarifa      NUMBER := 0;
    vnicmsretido    NUMBER := 0;
    vnicmsbnfretido NUMBER := 0;
    --vnicmsretido_parc1           NUMBER;
    vnprazoadicional_client      pcclient.prazoadicional%TYPE;
    vnprazoadicional_cli         pcclient.prazoadicional%TYPE;
    vnprazomedio                 pcclient.prazoadicional%TYPE;
    vnprazoadicional1            pcclient.prazoadicional%TYPE;
    vsutilizaprocessogeracaocpst VARCHAR2(1); --Tarefa 124971
    vsgeratitulost_client        pcclient.geratitulost%TYPE;
    vnvlminimost_client          pcclient.vlminimost%TYPE;
    vsvlminvendabk               pcparamfilial.valor%TYPE;
    vncredito                    NUMBER;
    -- BOOLEAN
    vbcrecli                BOOLEAN;
    vbnaogerarpcprest_cred  BOOLEAN;
    vbcobrartarifabanc      BOOLEAN := TRUE;
    VBPULOUPEDIDO           BOOLEAN := FALSE;
    vsisentotxboleto_client pcclient.isentotxboleto%TYPE;
    vsagregarvalorstdescfin pcclient.agregarvalorstdescfin%TYPE;
    -- PCCOB
    vsboleto             pccob.boleto%TYPE;
    vsboleto_cob         pccob.boleto%TYPE;
    vncodbancotarifa_cob pccob.codbancotarifa%TYPE;
    vspagcomissao_cob    pccob.pagcomissao%TYPE;
    --vnvalortarifas           NUMBER;
    visomatarifanf           NUMBER := 0;
    vnvlentrada              NUMBER := 0;
    vntotalgertemp           NUMBER := 0;
    vnperclanccredcli_consum pcconsum.perclanccredcli%TYPE;
    vReturnBaixaCredito      NUMBER;
    vsorgaopubfederal_client pcclient.orgaopubfederal%TYPE;
    vlretencao_orgpubfed     NUMBER;
    vnTotal_TV1Bnf           NUMBER := 0;
    vntotalger_ant           NUMBER := 0;
    vndifer                  NUMBER := 0;
    vn_total_financ          NUMBER := 0;
    vnvlfretea               NUMBER;
    vnvlfreteb               NUMBER := 0;
    vntotalipi               PCNFSAID.VLIPI%TYPE;
    vntotalvldifaliquotas    NUMBER := 0;
    vnvalorparcipi           NUMBER := 0;
    vnvalorparcst            NUMBER := 0;
    vndiferipi               NUMBER := 0;
    vndiferst                NUMBER;
    vnvalorparcdifaliquotas  NUMBER := 0;
    vnvalorparcfretea        NUMBER;
    vndiferfretea            NUMBER;
    vnvalorparcfreteb        NUMBER;
    vndiferfreteb            NUMBER;
    vnvalorparccont          NUMBER;
    vnvalorparccont1         NUMBER := 0;
    vndifercont              NUMBER;
    pddataemicao             DATE;
    vdcodcidade              pcclient.codcidade%TYPE;
    -- PCPrazoAdicional
    vnnumdias_prazoadicional pcprazoadicional.numdias%TYPE;
    vnnumregiao_praca        pcpraca.numregiao%TYPE;
    -- PCUSUARI
    vncodsupervisor_usuari       pcusuari.codsupervisor%TYPE;
    VNNUMPED                     NUMBER;
    vni                          NUMBER := 1;
    vnprest                      NUMBER;
    vnprestb                     VARCHAR2(1);
    vsbloqdesdemitentedif_prest  pcprest.bloqdesdemitentedif%TYPE;
    vsbloqdesdemitentedif_filial pcfilial.bloqdesdemitentedif%TYPE;
    pscodfilialnf                PCNFSAID.CODFILIALNF%TYPE;
    vscodcob_pedc                pcpedc.codcob%TYPE;
    vscodcob_prest               pcpedc.codcob%TYPE; -->Tarefa: 204140
    VN_VLTOT_PEDIDOS_VINC        NUMBER;
    pddataentregaproduto         date;
    pcodmotorista                PCNFSAID.CODMOTORISTA%TYPE;
    vnPERCOM_ENTRADA             NUMBER := 0;
    vnPERCOM_ENTRADA2            NUMBER := 0;
    vnPERCOM_ENTRADA3            NUMBER := 0;
    vnPERCOM_ENTRADA4            NUMBER := 0;
    vnPERCOMLIQ_ENTRADA          NUMBER := 0;
    vncomissao                   NUMBER := 0;
    vncomissao2                  NUMBER := 0;
    vncomissao3                  NUMBER := 0;
    vncomissao4                  NUMBER := 0;
    vipresttef_desc              INTEGER := 0;
    vnindicea                    NUMBER;
    vnindiceb                    NUMBER;
    vnindatx                     NUMBER;
    vnindbtx                     NUMBER;
    vnindatx1                    NUMBER;
    vnindbtx1                    NUMBER;
    ----------------SupplierCard---------------------
    vsCobSupplierCard       pccob.cobsuppliercard%type;
    vsCondFinanc            pcsuppliparamfat.condfinanc%type;
    vnIntervaloParcelasSupp pcsuppliparamfat.qtdiasvencparcela1%type := 0;
    --vnDtVenc1Supp                  pcsuppliparamfat.dtvenc1%type;
    --vnDtVenc2Supp                  pcsuppliparamfat.dtvenc1%type;
    --vnLimiteCredSuppli             pcclient.limitecredsuppli%type;
    vnCARENCIA                     NUMBER := 0;
    vnDIAFIXO                      NUMBER;
    vnNUMPARCELAS                  NUMBER;
    vicontador                     NUMBER := 0;
    vbdescboletoparc1              BOOLEAN := FALSE;
    vnperdescfin_client            pcclient.perdescfin%TYPE;
    pstipotratamentodescfin_client pcclient.tipotratamentodescfin%TYPE;
    vddtdesc_prest                 pcprest.dtdesc%TYPE;
    pnqtddiasaplicdescfin_client   pcclient.qtddiasaplicdescfin%TYPE;
    visomatarifapcprest            NUMBER;
    VSSOMATXBOLETO                 PCPREST.SOMATXBOLETO%TYPE;
    vndialimitvecimento            NUMBER;
    vdDIALIMITFATU                 NUMBER;
    vnDESCFIN_TEMP                 NUMBER;
    VNVALORCONT_TOTAL              NUMBER;
    vdvalorparcelarestricao        pcprest.valor%TYPE;
    viproxnumtransvendacpst        INTEGER;
    vc2serie_docc                  pcdocc.serie%TYPE;
    vnicmsretidognre               NUMBER := 0;
    vnVLSEGURO                     NUMBER;
    -- Início - 663.069452.2015
    VNVALORTOTALST     NUMBER;
    VNVALORBASEST      NUMBER;
    VNVALOROUTRAS      NUMBER;
    VNQT_CONTASRECEBER NUMBER;
    -- Fim - 663.069452.2015
    v_numlanc                NUMBER;
    vncodfornec              NUMBER;
    vntotalcontasrecebercont NUMBER;
    vntotalitens             NUMBER;
    vntotalitenscont         NUMBER;
    vnvloutros               NUMBER;
    vnvloutroscont           NUMBER;
    vntotvldesconto          NUMBER := 0;
    vnvalorprest1            pcprest.valor%TYPE;
    vnvalorprest2            pcprest.valor%TYPE;
    vntotalcontasreceber     NUMBER;
    G_VNTXBOLETO             NUMBER;
    vnnumregiaoclinf         NUMBER;
    VN_NUMNOTA               NUMBER;
    VN_NUMTRANSVENDA         NUMBER;
    VN_VLBONIFIC             NUMBER;
    CAB                       FATURAMENTO.C_PEDIDOS%ROWTYPE;
    VN_NUMPEDPGTOPEDIDO      PCPEDC.NUMPED%TYPE;

    vnPCDIASPLPAGCLI         NUMBER;
    VNTOTDIASPLPAGCLI        NUMBER;
    VSCODCOBSEFAZ            PCCOB.CODCOBSEFAZ%TYPE;
    vsUTILIZARECALCULOSTCHECKOUT PCFILIAL.UTILIZARECALCULOSTCHECKOUT%TYPE;
    vPercvenpcre      NUMBER := 0;

    PROCEDURE MUDAVALPEDIDO(P_FORMAPGTOPEDIDO IN PCFORMAPGTOPEDIDO%ROWTYPE) IS
    BEGIN

      /*SELECT NVL(MAX(PREST),0)
        INTO vnprest
        FROM PCPREST
       WHERE NUMTRANSVENDA = PN_NUMTRANSVENDA;

      vnprest                := vnprest + 1;   */
      CAB.CODCOB             := P_FORMAPGTOPEDIDO.CODCOB;
      CAB.CODPLPAG           := P_FORMAPGTOPEDIDO.CODPLPAG;
      CAB.VLENTRADA          := P_FORMAPGTOPEDIDO.VLENTRADA;
      ROW_PCNFSAID.CODCOB    := P_FORMAPGTOPEDIDO.CODCOB;
      ROW_PCNFSAID.VLTOTGER  := P_FORMAPGTOPEDIDO.VALOR;  /*- NVL(P_FORMAPGTOPEDIDO.VLRTROCO, 0) - NVL(P_FORMAPGTOPEDIDO.VLENTRADA, 0) TROCO É GERADO UMA PARCELA A PARTE*/
      ROW_PCNFSAID.DTENTREGA := P_FORMAPGTOPEDIDO.DTVENC;

      SELECT PCPLPAG.TIPOENTRADA
        INTO cab.tipoentrada_plpag
        FROM PCPLPAG
       WHERE CODPLPAG = P_FORMAPGTOPEDIDO.CODPLPAG;
    END;
  BEGIN
    -----------------------------------------------------------------------------------------

    BEGIN
      VNQT_CONTASRECEBER := 0; -- 663.069452.2015 - variável para validação de quantas vezes <<GERAR_CONTASRECEBER>> foi chamado.
      -- 663.069452.2015 -Ponto para chamada do processamento de contas a receber utilizando o método GOTO
      <<GERAR_CONTASRECEBER>>
      SAVEPOINT CONTASRECEBER; -- 663.069452.2015 - Salvando o ponto até onde será dado o rollback.


      ------------------------------ CARREGANDO OS VALORES DO PEDIDO E DA NOTA -------------------------------------
      SELECT S.*
        INTO ROW_PCNFSAID
        FROM PCNFSAID S
       WHERE NUMTRANSVENDA = PN_NUMTRANSVENDA;

      CAB := P_CAB;
      -------------------------------- CARREGANDO OS VALORES DO PEDIDO E DA NOTA -----------------------------------

      -------------------------------- CARREGA OS VALORES DO PLANO DE PGTO COM MÚLTIPLAS COBRANCAS -----------------------------------
      -- HIS.00127.2017
      VN_NUMPEDPGTOPEDIDO := NVL(P_FORMAPGTOPEDIDO.NUMPED, 0);
      IF VN_NUMPEDPGTOPEDIDO > 0 THEN
        MUDAVALPEDIDO(P_FORMAPGTOPEDIDO);
      END IF;
      -------------------------------- CARREGA OS VALORES DO PLANO DE PGTO COM MÚLTIPLAS COBRANCAS -----------------------------------

      vntotalger := CASE
                      WHEN ROW_PCNFSAID.CONDVENDA IN (5, 11) THEN
                        ROW_PCNFSAID.VLTOTAL
                      WHEN (ROW_PCNFSAID.CONDVENDA IN (14)) AND (ROW_PCNFSAID.CODCOB LIKE 'BNF%') THEN
                        ROW_PCNFSAID.VLTOTAL
                      ELSE
                        ROW_PCNFSAID.VLTOTGER
                    END;
      vnvloutrasdesp           := ROW_PCNFSAID.VLOUTRASDESP;
      vntotalipi               := ROW_PCNFSAID.VLIPI;
      vntotalvldifaliquotas    := ROW_PCNFSAID.totdifaliquotas;
      pddataemicao             := ROW_PCNFSAID.DTSAIDA;
      VNNUMPED                 := ROW_PCNFSAID.NUMPED;
      pscodfilialnf            := NVL(ROW_PCNFSAID.CODFILIALNF,
                                      ROW_PCNFSAID.CODFILIAL);
      /*VEN-2059*/
      if FERRAMENTAS.F_BUSCARPARAMETRO_ALFA('DTBASEVENCTITULOFATURAMENTO', ROW_PCNFSAID.CODFILIAL, 'DE') = 'DE' then
        pddataentregaproduto     := ROW_PCNFSAID.DTENTREGA;
      else
        pddataentregaproduto     := trunc(sysdate);
      end if;

      pcodmotorista            := ROW_PCNFSAID.CODMOTORISTA;
      pvncodigocobranca        := ROW_PCNFSAID.CODCOB;
      VNICMSRETIDOGNRE         := ROW_PCNFSAID.ICMSRETIDOGNRE;
      vnVLSEGURO               := ROW_PCNFSAID.Vlseguro;
      vncomissao               := ROW_PCNFSAID.COMISSAO;
      vncomissao2              := ROW_PCNFSAID.COMISSAO2;
      vncomissao3              := ROW_PCNFSAID.COMISSAO3;
      vncomissao4              := ROW_PCNFSAID.COMISSAO4;
      vddatavenc_anterior      := NULL;
      VBCRECLI                 := TRUE;
      VBCOBRARTARIFABANC       := TRUE;
      G_VNTXBOLETO             := FATURAMENTO.VALORTARIFA(CAB.NUMPED,
                                                          CAB.VLATEND,
                                                          P_FORMAPGTOPEDIDO);
      vnperclanccredcli_consum := 100; -- DESCONTINUADO
      vntotalger_ant           := 0;
      vntotalitens             := 0;
      vntotalitenscont         := 0;
      vnvloutros               := 0;
      vnvloutroscont           := 0;
      vntotvldesconto          := 0;
      VN_NUMNOTA               := ROW_PCNFSAID.NUMNOTA;
      VN_NUMTRANSVENDA         := ROW_PCNFSAID.NUMTRANSVENDA;
      VN_VLBONIFIC             := ROW_PCNFSAID.VLBONIFIC;

      /*************************************************************************************
       * Caso esteja sendo gerado parcela de boleto, e seja multiplas formas de pagamentos *
       * vai somar ao valor da cobrança, a taxa de boleto, caso sistema esteja parametriado*
       * para somar tarifa bancária ao valor da prestação de contas a receber.             *
       *************************************************************************************/
      IF (nvl(P_FORMAPGTOPEDIDO.NUMPED, 0) > 0) AND
         (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S') AND
         (VNQT_CONTASRECEBER = 0)
      THEN
        vntotalger := vntotalger + G_VNTXBOLETO;
      END IF;

      IF (VNQT_CONTASRECEBER > 0) THEN
        G_VNTXBOLETO := 0;
      END IF;

      BEGIN
        SELECT nvl(sum(ROUND(qt * pvenda, 2)), 0)
          INTO VN_VLTOT_PEDIDOS_VINC
          FROM pcpedi
         WHERE numped IN (SELECT numped
                            FROM pcpedc
                           WHERE numpedorigem = CAB.NUMPEDORIGEM
                              OR numped = VNNUMPED);
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          VN_VLTOT_PEDIDOS_VINC := 0;
      END;

      -- VERIFICANDO SE EXISTE CRÉDITO A CONCEDER
      CASE FATURAMENTO.VERIFICAR_CRECLI(CAB.NUMPED)
        WHEN 'P' THEN
          VBCRECLI := TRUE;
        WHEN 'T' THEN
          --Remover taxa de boleto quando o valor total do pedido for utilizado como crédito
          VBCRECLI           := TRUE;
          G_VNTXBOLETO       := 0;
          VBCOBRARTARIFABANC := FALSE;
        ELSE
          VBCRECLI := FALSE;
      END CASE;

      SELECT codbancotarifa, NVL(boleto, 'N'), CODCOBSEFAZ
        INTO vncodbancotarifa_cob, vsboleto_cob, VSCODCOBSEFAZ
        FROM pccob
       WHERE pccob.codcob =
             DECODE(cab.codcob, 'BK', pvncodigocobranca, cab.codcob);
      --pvncodigocobranca

      ------------- DADOS DO CLIENTE

      SELECT NVL(ORGAOPUBFEDERAL, 'N'),
             codcidade,
             NVL(perdescfin, 0),
             NVL(tipotratamentodescfin, 'DV'),
             NVL(qtddiasaplicdescfin, 0),
             NVL(prazovencst, 0),
             NVL(geratitulost, 'N'),
             NVL(prazoadicional, 0),
             (SELECT numregiao
                FROM pcpraca
               WHERE codpraca = pcclient.codpraca),
             NVL(AGREGARVALORSTDESCFIN, 'N')
        INTO vsorgaopubfederal_client,
             vdcodcidade,
             vnperdescfin_client,
             pstipotratamentodescfin_client,
             pnqtddiasaplicdescfin_client,
             viprazovencst_client,
             vsgeratitulost_client,
             vnprazoadicional_client,
             vnnumregiaoclinf,
             VSAGREGARVALORSTDESCFIN
        FROM PCCLIENT
       WHERE CODCLI = ROW_PCNFSAID.CODCLI;

      IF cab.usadescfinanceiro = 'N' THEN
        vnperdescfin_client := 0;
      END IF;
      IF (cab.usasaldocontacorrentedescfin = 'S') THEN
        IF cab.perdescfin > 0 THEN
          vnperdescfin_client := cab.perdescfin;

          IF cab.valordescfin > 0 THEN
            IF cab.vlatend > 0 THEN
              vnperdescfin_client := ROUND((cab.valordescfin / cab.vlatend) * 100,
                                           2);
            END IF;
          END IF;

        END IF;
      END IF;

      -- INÍCIO - DADOS AUXILIARES
      buscardadosauxiliares(VNNUMPED,
                            vsboleto,
                            vncodsupervisor_usuari,
                            vnnumregiao_praca,
                            pvc2menssagen,
                            P_FORMAPGTOPEDIDO);

      IF cab.numregiao IS NOT NULL -- Tarefa 50552
       THEN
        vnnumregiao_praca := cab.numregiao;
      END IF;

      IF (cab.codclinf > 0) AND (cab.codcli <> cab.codclinf) AND
         (vnnumregiaoclinf > 0) THEN
        vnnumregiao_praca := vnnumregiaoclinf;
      END IF;
      -- FIM - DADOS AUXILIARES

      -- condição para o plano de pagamento com valor de entrada
      IF cab.tipoentrada_plpag = 3 THEN
        vnvlentrada := nvl(P_FORMAPGTOPEDIDO.VLENTRADA, cab.vlentrada);
      END IF;

      -- INÍCIO - VALIDAÇÕES SUPPLIERCARD (HIS.00023.2017)-----------------------------------------
      BEGIN
        SELECT NVL(PCCOB.COBSUPPLIERCARD, 'N')
          INTO VSCOBSUPPLIERCARD
          FROM PCPEDC, PCCOB
         WHERE PCPEDC.CODCOB = PCCOB.CODCOB
           AND PCPEDC.NUMPED = CAB.NUMPED;
      EXCEPTION
        WHEN OTHERS THEN
          VSCOBSUPPLIERCARD := 'N';
      END;

      IF VSCOBSUPPLIERCARD = 'S' THEN
        BEGIN
          SELECT SF.CONDFINANC, SF.QTDIASVENCPARCELA1
            INTO VSCONDFINANC, VNINTERVALOPARCELASSUPP
            FROM PCSUPPLIPARAMFAT SF, PCSUPPLICLIENTE SC
           WHERE SF.CONDFINANC = CAB.CONDFINANC
             AND SF.TIPOCLIENTE = SC.TIPOCLIENTE
             AND SF.CONDFINANC = CAB.CONDFINANC
             AND SF.PLANO = CAB.PLANOSUPPLI
             AND TRUNC(SF.DTFATURAMENTO) = TRUNC(SYSDATE)
             AND SC.CODCLI = CAB.CODCLI;
        EXCEPTION
          WHEN OTHERS THEN
            FATURAMENTO.VRPARAMETROS.NUMPED := CAB.NUMPED;
            FATURAMENTO.VRPARAMETROS.DATA   := SYSDATE;
            FATURAMENTO.VRPARAMETROS.MSG    := 'NÃO FOI LOCALIZADA INFORMAÇÕES DA COBRANÇA SUPPLIERCARD PARA ESTE PEDIDO/CLIENTE. NÃO É POSSÍVEL CONTINUAR O FATURAMENTO';
            PVC2MENSSAGEN                   := FATURAMENTO.VRPARAMETROS.MSG;
            FATURAMENTO.GRAVARLOG(FATURAMENTO.VRPARAMETROS,
                                  (CASE WHEN FATURAMENTO.VRPARAMETROS.MSG LIKE
                                   '%ORA-%' THEN
                                   DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                   TO_CHAR($$PLSQL_LINE) END));
            VBPULOUPEDIDO := TRUE;
            ---  GOTO FIM_LOOP_CAB;
        END;
      END IF;
      -- FIM - VALIDAÇÕES SUPPLIERCARD (HIS.00023.2017)-----------------------------------------

      SELECT NVL(bloqdesdemitentedif, 'N')
        INTO vsbloqdesdemitentedif_filial
        FROM pcfilial
       WHERE codigo = pscodfilialnf;

      FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
      FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
      FATURAMENTO.vrPARAMETROS.MSG    := 'Iniciando o contas a receber. vntotalger = ' ||
                                         vntotalger;
      FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, $$PLSQL_LINE);


      IF cab.condvenda NOT IN (4, 8, 13, 20) THEN
        vnnumparc  := 1;
        vddtvenc1  := NULL;
        vddtvenc2  := NULL;
        vddtvenc3  := NULL;
        vddtvenc4  := NULL;
        vddtvenc5  := NULL;
        vddtvenc6  := NULL;
        vddtvenc7  := NULL;
        vddtvenc8  := NULL;
        vddtvenc9  := NULL;
        vddtvenc10 := NULL;
        vddtvenc11 := NULL;
        vddtvenc12 := NULL;

        IF cab.codplpag <> 99 THEN
          SELECT
          -- Tarefa 33753 ...
           DECODE(NVL(numparcelas, 0), 0, 1, numparcelas),
           NVL(numdiascarencia, 0),
           NVL(formaparcelamento, 'C'),
           -- ... Tarefa 33753
           prazo1,
           prazo2,
           prazo3,
           prazo4,
           prazo5,
           prazo6,
           prazo7,
           prazo8,
           prazo9,
           prazo10,
           prazo11,
           prazo12,
           dtvenc1,
           dtvenc2,
           dtvenc3
            INTO -- Tarefa 33753 ...
                 vnnumparc,
                 vnnumdiascarencia,
                 vsformaparcelamento_plpag,
                 -- ... Tarefa 33753
                 vnprazo1,
                 vnprazo2,
                 vnprazo3,
                 vnprazo4,
                 vnprazo5,
                 vnprazo6,
                 vnprazo7,
                 vnprazo8,
                 vnprazo9,
                 vnprazo10,
                 vnprazo11,
                 vnprazo12,
                 vddtvenc1,
                 vddtvenc2,
                 vddtvenc3
            FROM pcplpag
           WHERE codplpag = cab.codplpag;
        END IF;

        IF cab.codplpag = 99 THEN
          vsformaparcelamento_plpag := 'C';
          -- Tarefa 44743
          vnprazo1  := cab.prazo1;
          vnprazo2  := cab.prazo2;
          vnprazo3  := cab.prazo3;
          vnprazo4  := cab.prazo4;
          vnprazo5  := cab.prazo5;
          vnprazo6  := cab.prazo6;
          vnprazo7  := cab.prazo7;
          vnprazo8  := cab.prazo8;
          vnprazo9  := cab.prazo9;
          vnprazo10 := cab.prazo10;
          vnprazo11 := cab.prazo11;
          vnprazo12 := cab.prazo12;
          vddtvenc1 := cab.dtvenc1;
          vddtvenc2 := cab.dtvenc2;
          vddtvenc3 := cab.dtvenc3;
        END IF;
        -- Tarefa 33753 - se for Parcelamento Mensal vnnumparc = campo numparcelas da PCPLPAG

        BEGIN
          vnnumparc := NVL(FATURAMENTO.calcularnumparcelas(cab.codplpag,
                                                           cab.numped,
                                                           P_FORMAPGTOPEDIDO),
                           0);

          --HIS.00127.2017 - Guilherme Freitas
          /************************************************************************************
           * Caso seja identificado                                                           *
           * a utilização de multiplos planos de pagamento e o existe bonificação junto ao    *
           * pedido, não irá gerar a parcela de bonificação juntos às parcelantes anteriores  *
           ************************************************************************************
          IF (VN_NUMPEDPGTOPEDIDO > 0) AND
             (NVL(P_FORMAPGTOPEDIDO.CODCOB, 'BNF') = 'BNF') AND
             (vnTotal_TV1Bnf >= 0)
          THEN
            vnnumparc := vnprest;
          END IF;       */

          IF vnnumparc = 0 THEN
            vnnumparc := 1;

            INSERT INTO PCLOGFATUR
              (dtfaturamento, numnota, numped, dslog)
            VALUES
              (SYSDATE,
               VN_NUMNOTA,
               cab.numped,
               'Atenção! O plano de pagto ' || TO_CHAR(cab.codplpag) ||
               ' não possui número de parcelas válido.' || chr(13) ||
               'O faturamento assumirá automaticamente como parcela única. Verifique!');

          END IF;
        EXCEPTION
          WHEN OTHERS THEN
            pvc2menssagen := 'Erro ao calcular número de parcelas no contas a receber. Verifique log de faturamento! ' ||
                             ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
            RAISE;
        END;
        --Fim cálculo qtde de parcelas

        /*        vntotalger := ROUND((NVL(vntotalger, 0) + -- -
        --vntotaldesconto -  Solicitação: 7.027851.2014
        --NVL(vnvlvendacesta,0) +
        -- - NVL(cab.vldesconto, 0) +  Solicitação: 7.027851.2014
        (NVL(cab.vlfrete, 0) \*+ NVL(vnvloutrasdesp, 0)*\)
        ),
        2);*/

        vnvalorparc := TRUNC((vntotalger / vnnumparc), 2);
        vnjuros     := 0;

        IF vrPARAMETROS.CON_COBRARVLTARIFAPARC1 = 'S' THEN
          vnnumparccob := vnnumparc;
        ELSE
          vnnumparccob := vnnumparc - 1;
        END IF;

        /*IF (vrPARAMETROS.USATXBOLETOAPENASUMANFMESMOCAR = 'S') AND
          --(verificar_txboleto(CAB.NUMPED))
           (NOT FATURAMENTO.ENCONTROUPED_TXAPENASUMANF(CAB.NUMPED)) THEN
          vnjuros    := 0;
          vnvltarifa := 0;
        ELSE   */
          --Tarefa: 185998
          IF vnnumparccob > 0 THEN
            vnjuros    := G_VNTXBOLETO / vnnumparccob;
            vnvltarifa := G_VNTXBOLETO;
          ELSE
            vnjuros    := 0;
            vnvltarifa := 0;
          END IF;
          --Fim Tarefa: 185998
        --END IF;

        --Tarefa 146201

        /*Tarefa: 197968
        SELECT tipovlminvendabk
          INTO vstipovlminvendabk
          FROM pcconsum;
        */

        -- Tarefa: 205247
        BEGIN
          SELECT SUM(DECODE(NVL(PCMOVCOMPLE.BONIFIC, 'N'),
                            'F',
                            PCMOV.QTCONT,
                            'S',
                            PCMOV.QTCONT,
                            0) * NVL(PCMOV.ST, 0))
            INTO VNICMSBNFRETIDO
            FROM PCMOV, PCMOVCOMPLE
           WHERE PCMOV.NUMTRANSITEM = PCMOVCOMPLE.NUMTRANSITEM
             AND PCMOV.NUMTRANSVENDA = VN_NUMTRANSVENDA;

          SELECT NVL(ICMSRETIDO, 0)
            INTO VNICMSRETIDO
            FROM PCNFSAID
           WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

         IF vnicmsbnfretido > 0 THEN
           vnicmsretido := vnicmsretido - vnicmsbnfretido;
         END IF;

        EXCEPTION
          WHEN NO_DATA_FOUND THEN

            SELECT NVL(SUM(ST * QTCONT), 0)
              INTO VNICMSRETIDO
              FROM PCMOV
             WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

          WHEN OTHERS THEN
            SELECT NVL(SUM(ST * QTCONT), 0)
              INTO VNICMSRETIDO
              FROM PCMOV
             WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;
        END;
        VNICMSRETIDO := TRUNC(VNICMSRETIDO, 2);

        -- Fim Tarefa: 205247

        IF (VRPARAMETROS.CODCOBST IS NOT NULL) AND
          --Tarefa: 185442
           (CAB.CONDVENDA NOT IN (5, 6, 12)) AND
          --Fim Tarefa: 185442
           (VSGERATITULOST_CLIENT = 'S') THEN

          VSUTILIZAPROCESSOGERACAOCPST := 'S';

          -- Tarefa 132501
          IF NOT (((VNICMSRETIDO > 0) OR (vnicmsbnfretido > 0))AND
              ((VNVLMINIMOST_CLIENT IS NULL) OR
              ((VNVLMINIMOST_CLIENT IS NOT NULL) AND
              (VNICMSRETIDO >= VNVLMINIMOST_CLIENT)))) THEN
            VSUTILIZAPROCESSOGERACAOCPST := 'N';
          END IF;
          -- Fim tarefa 132501
        ELSE
          VSUTILIZAPROCESSOGERACAOCPST := 'N';
        END IF;

        --HIS.00127.2017 - Guilherme Freitas
        /************************************************************************************
         * Caso seja identificado    *
         * a utilização de multiplos planos de pagamento e o sistema esteja parametrizado  *
         * para geração de uma parcela de ST, será forçado para não gerar parcela de ST     *
         * junto às parcelas comuns.  *
         ************************************************************************************/
        IF (NVL(P_FORMAPGTOPEDIDO.CODCOB, vrPARAMETROS.CODCOBST) <> vrPARAMETROS.CODCOBST) AND
           (NVL(vsutilizaprocessogeracaocpst, 'N') = 'S') THEN
          VSUTILIZAPROCESSOGERACAOCPST := 'N';
        END IF;

        IF NVL(vrPARAMETROS.CON_TIPOTXBOLETO, 'P') = 'F' THEN

          SELECT pcparamfilial.valor
            INTO vsvlminvendabk
            FROM pcparamfilial
           WHERE pcparamfilial.nome LIKE 'VLMINVENDABKFILIAL'
             AND codfilial = cab.codfilial;

        ELSE

          SELECT pcparamfilial.valor
            INTO vsvlminvendabk
            FROM pcparamfilial
           WHERE pcparamfilial.nome LIKE 'CON_VLMINVENDABK'
             AND codfilial = '99';

        END IF;

        -- Tarefa 60162 (19/03/2008 - Pablo)
        -- Ajuste para não acrescentar boleto na NF quando Crédito superior ou igual NF
        FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
        FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
        FATURAMENTO.vrPARAMETROS.MSG    := 'Verificando créditos do cliente(PCCRECLI)...';

        -- 14.01 -- Verifica Creditos para este cliente -- // Tarefa: 213245
        VNCREDITO := 0;

        FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                              (CASE WHEN
                               FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN
                               DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                               TO_CHAR($$PLSQL_LINE) END));

        IF (vnjuros > 0) AND (vbcrecli) -- Tarefa 122597
         THEN
          SELECT NVL(SUM(tab.valor), 0) valor
            INTO vncredito
            FROM (SELECT valor
                    FROM pccrecli
                   WHERE codcli = cab.codcli
                     AND dtdesconto IS NULL
                     AND codfilial = cab.codfilial
                     AND numped = cab.numped
                     AND situacao is null
                  UNION ALL
                  SELECT valor
                    FROM pccrecli
                   WHERE codcli = cab.codcli
                     AND situacao is null
                     AND dtdesconto IS NULL
                     AND codfilial = cab.codfilial
                     AND NVL(numped, 0) = 0) tab;

          IF vncredito >= vntotalger THEN
            vnjuros := 0;
          ELSE
            -- Tarefa 61490
            -- Validar se caso use o Crédito do cliente ainda cobrará Boleto nas parcelas
            vnvalorparc := TRUNC(((vntotalger - vncredito) / vnnumparc), 2);

            IF ((vnvalorparc < vrPARAMETROS.CON_VLMAXTARIFABANC) OR
               (vnvalorparc > vrPARAMETROS.CON_VLMINTARIFABANC)) THEN
              vnjuros := 0;
            END IF;
          END IF;
        END IF;

        -- Clientes Isentos de Taxa do Boleto Bancario
        IF vsISENTOTXBOLETO_CLIENT = 'S' THEN
          vnJUROS    := 0;
          vnVLTARIFA := 0;
        END IF;

        -- Tarefa 61358
        IF vsboleto_cob = 'N' THEN
          vnjuros    := 0;
          vnvltarifa := 0;
        END IF;

        --Tarefa 32428
        visomatarifanf := 0;

        IF (cab.codcob = 'BK') OR (vsboleto = 'S') THEN
          IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S' THEN

            visomatarifanf := 1;

          END IF;
        ELSE
          -- Se a tarifa nao for cobrada zerar variavel
          vnjuros := 0;
        END IF;

        -- Tarefa: 1337.022881.2014 - Zerar o credito para evitar duplicidade de valores
        VNCREDITO := 0;

        /*Tarefa: 208652 -> Validação substituída pela função "VERIFICAR_CRECLI"
        IF ((vrPARAMETROS.CON_NAOUSARPCCRECLICONSFINAL = 'S') AND
           ((cab.codcli = 1) OR (cab.codcli = 2) OR
           (cab.codcli = 3))) THEN
          vbcrecli := FALSE;
        END IF;*/

        IF (CAB.CODCOB NOT IN ('BNF', 'BNFT', 'BNTR', 'BNFR', 'BNRP')) AND
           (vbcrecli = TRUE) THEN
          DECLARE
            vnCREDITOPEDIDO NUMBER := 0;
            vnCREDITOSEMPED NUMBER := 0;
            vnCREDITOORGPUB NUMBER := 0;
          BEGIN

             vntotalgertemp := ROUND(((vntotalger ) *
                                    (vnperclanccredcli_consum / 100)),
                                    2);

            --Se utilizar parcela de ST separado retirar do montante gerencial
            --para utilização de crédito.
            IF vsUTILIZAPROCESSOGERACAOCPST = 'S' THEN

              VNTOTALGERTEMP := VNTOTALGERTEMP - VNICMSRETIDO;

            END IF;

            -- Fim 14.1
            IF NOT (VN_NUMPEDPGTOPEDIDO > 0) THEN -- HIS.00127.2017
              IF (FERRAMENTAS.F_BUSCARPARAMETRO_ALFA('UTILCREDCLIFATVENDAWEB',
                                                     '99',
                                                     'S') = 'S') or
                 (NOT CAB.ORIGEMPED = 'W') THEN
                IF (cab.geracp = 'N') THEN
                  IF (vntotalgertemp > 0) THEN

                    --Credito devido a indenizacoes - NUMPED É PASSADO COMO PARÂMETRO
                    vReturnBaixaCredito := pkg_creditocliente.FNC_BAIXARCREDITOCLIENTE(CAB.codfilial,
                                                                                       CAB.codcli,
                                                                                       0, --cred.numnota,
                                                                                       VN_NUMTRANSVENDA,
                                                                                       CAB.NUMPED,
                                                                                       vntotalgertemp, -- -cred.valor,
                                                                                       1400,
                                                                                       FATURAMENTO.G_FUNC.MATRICULA);

                    IF vReturnBaixaCredito = vntotalgertemp THEN
                     vnCREDITOPEDIDO := 0; -- Não teve baixa de crédito
                    ELSIF vReturnBaixaCredito > 0 THEN
                      vnCREDITOPEDIDO := vntotalgertemp - vReturnBaixaCredito; -- Baixa parcial
                    ELSIF vReturnBaixaCredito = 0 THEN
                      vnCREDITOPEDIDO := vntotalgertemp; -- Baixa Total, pode ter havido sobra
                    END IF;

                   -- Credito nao relacionado a indenizacoes (Retorna saldo a baixar)
                    IF (VRETURNBAIXACREDITO > 0) THEN

                      vReturnBaixaCredito :=  pkg_creditocliente.FNC_BAIXARCREDITOCLIENTE(CAB.codfilial,
                                                                                        CAB.codcli,
                                                                                        0, --cred.numnota,
                                                                                        VN_NUMTRANSVENDA,
                                                                                        0,
                                                                                        vReturnBaixaCredito, -- -cred.valor,
                                                                                        1400,
                                                                                        FATURAMENTO.G_FUNC.MATRICULA);

                      IF vReturnBaixaCredito = (vntotalgertemp - vnCREDITOPEDIDO) THEN
                        vnCREDITOSEMPED := 0; -- Não teve baixa de crédito
                      ELSIF vReturnBaixaCredito > 0 THEN
                        vnCREDITOSEMPED := vntotalgertemp - ( vReturnBaixaCredito + vnCREDITOPEDIDO); -- Baixa parcial
                      ELSIF vReturnBaixaCredito = 0 THEN
                        vnCREDITOSEMPED := vntotalgertemp - vnCREDITOPEDIDO; -- Baixa Total, pode ter havido sobra
                      END IF;
                    ELSE
                      vnCREDITOSEMPED := 0;
                    END IF;

                  END IF;
                END IF;
                -- Fim 14.2
              END IF;
            END IF;

            IF (FERRAMENTAS.F_BUSCARPARAMETRO_ALFA('UTILCREDCLIFATVENDAWEB',
                                                     '99',
                                                     'S') = 'S') or
                 (NOT CAB.ORIGEMPED = 'W') THEN
                --Tarefa 56706 - Crédito relacionado a Retenção para Órgão Público Federal
               IF (cab.geracp = 'N') THEN
                  IF (vsorgaopubfederal_client = 'S') THEN
                    SELECT NVL(SUM((NVL(pcmov.punitcont, 0) *
                                   (NVL(pcprodfilial.perdescretencao, 0) / 100)) *
                                   pcmov.qtcont),
                               0)
                      INTO vlretencao_orgpubfed
                      FROM pcprodfilial, pcmov
                     WHERE pcprodfilial.codfilial = cab.codfilial
                       AND pcprodfilial.codprod = pcmov.codprod
                       AND pcmov.numtransvenda = VN_NUMTRANSVENDA;

                    IF vlretencao_orgpubfed > 0 THEN
                      vncredito := vlretencao_orgpubfed;

                      vReturnBaixaCredito := pkg_creditocliente.FNC_BAIXARCREDITOCLIENTE(cab.codfilial,
                                                                                         cab.codcli,
                                                                                         VN_NUMNOTA,
                                                                                         VN_NUMTRANSVENDA,
                                                                                         cab.numped,
                                                                                         vlretencao_orgpubfed,
                                                                                         1400,
                                                                                         FATURAMENTO.G_FUNC.MATRICULA);

                      IF vReturnBaixaCredito > 0 THEN
                        vnCREDITOORGPUB := vntotalgertemp - vReturnBaixaCredito; -- Baixa parcial
                      ELSIF vReturnBaixaCredito = 0 THEN
                        vnCREDITOORGPUB := vntotalgertemp; -- Baixa Total, pode ter havido sobra
                      elsIF vReturnBaixaCredito = vntotalgertemp THEN
                        vnCREDITOORGPUB := 0; -- Não teve baixa de crédito
                      END IF;


                    END IF;
                  END IF;
              END IF;
            END IF;

            VNCREDITO := vnCREDITOPEDIDO + vnCREDITOSEMPED + vnCREDITOORGPUB;
          END;
        END IF;
        --Fim-Tarefa 56706

        BEGIN
          vnTotal_TV1Bnf := 0;

          SELECT --ROUND(NVL(SUM(NVL(PBONIFIC,0) * QT),0),2)
          -- 2311.073877.2016
           NVL(SUM(ROUND((CASE
                           WHEN NVL(PCPEDI.BONIFIC, 'N') = 'F' THEN
                            ( NVL(PCPEDI.PVENDA,  NVL(PCPEDI.PBONIFIC, 0)))
                           ELSE
                            NVL(PCPEDI.PBONIFIC, 0)
                         END) * QT,
                         2)),
               0) PBONIFIC
          -- 2311.073877.2016
            INTO VNTOTAL_TV1BNF
            FROM PCPEDI, PCPEDC
           WHERE NVL(BONIFIC, 'N') = 'F'
             and PCPEDC.CONDVENDA IN (1, 14)
             AND PCPEDI.NUMPED = PCPEDC.NUMPED
             AND PCPEDC.NUMPED = CAB.NUMPED;

          IF NVL(VNICMSBNFRETIDO,0) > 0 and VSUTILIZAPROCESSOGERACAOCPST = 'S' THEN
            VNTOTAL_TV1BNF := VNTOTAL_TV1BNF - NVL(VNICMSBNFRETIDO, 0);
          END IF;
        EXCEPTION
          WHEN OTHERS THEN
            vnTotal_TV1Bnf := 0;
        END;

        --HIS.00127.2017 - Guilherme Freitas
        /************************************************************************************
         * Caso seja identificado                                                           *
         * a utilização de multiplos planos de pagamento e o existe bonificação junto ao    *
         * pedido, não irá gerar a parcela de bonificação juntos às parcelantes anteriores  *
         ************************************************************************************/
        IF (NVL(P_FORMAPGTOPEDIDO.CODCOB, 'BNF') <> 'BNF') AND
           (vnTotal_TV1Bnf >= 0)
        THEN
          vnTotal_TV1Bnf := 0;
        ELSIF (NVL(P_FORMAPGTOPEDIDO.CODCOB, 'XX') = 'BNF')
        THEN
          vnTotal_TV1Bnf := P_FORMAPGTOPEDIDO.VALOR;
        END IF;

        VNTOTALGER_ANT := VNTOTALGER;
        VNTOTALGER     := VNTOTALGER - VNCREDITO -
                          (VISOMATARIFANF * (VNJUROS * VNNUMPARCCOB)) --HIS.02571.2015
         ;

        -- Tarefa 103223
        IF vnvlentrada > vntotalger THEN
          vnvlentrada := vntotalger;
        END IF;

        vntotalger := vntotalger - vnvlentrada;

        -- Fim tarefa 103223

        IF ((VNTOTALGER + (VISOMATARIFANF * (VNJUROS * VNNUMPARCCOB))) - G_VNTXBOLETO) <= 0
            and (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S')THEN
          VNTOTALGER     := 0;
          VNVLOUTRASDESP := VNVLOUTRASDESP - G_VNTXBOLETO;

          IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S' THEN

            UPDATE PCNFSAID
               SET VLOUTRASDESP =
                   (VLOUTRASDESP - G_VNTXBOLETO),
                   VLTOTAL     =
                   (VLTOTAL - G_VNTXBOLETO),
                   VLTOTGER    =
                   (VLTOTGER - G_VNTXBOLETO)
             WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

          END IF;

          G_VNTXBOLETO := 0;

        END IF;

        -- Tarefa 47656
        IF cab.conciliaimportacao = 'S' THEN
          vntotalger := 0;

          SELECT ROUND(SUM((pcpedi.pvenda - pclote.precocompra) * pcpedi.qt),
                       2)
            INTO vntotalger
            FROM pcpedi, pclote
           WHERE pcpedi.numped = cab.numped
             AND pcpedi.codprod = pclote.codprod
             AND pcpedi.numlote = pclote.numlote
             AND NVL(pcpedi.codfilialretira, cab.codfilial) =
                 pclote.codfilial
             AND pcpedi.pvenda > pclote.precocompra;
        END IF;
        -- Fim Tarefa 47656

        -- Tarefa 60308: Tratamento de rateio de frete em X parcelas
        vnvlfretea := 0;
        vnvlfreteb := 0;

        IF vrPARAMETROS.CON_LANCARFRETEXPARCELAS = 'S'
           and (vntotalger - cab.vlfrete ) > 0 THEN
          vnvlfretea := NVL(cab.vlfrete, 0);
          vnvlfreteb := NVL(cab.vlfrete, 0) - vnvlfretea;
        END IF;

        -- Parcelas B
        vnvalorparc := TRUNC(((vntotalger - vnvlfreteb) / vnnumparc), 2);
        vndifer     := ((vntotalger - vnvlfreteb) -
                       (vnvalorparc * vnnumparc));
        -- Tarefa 60308: Tratamento de rateio de impostos em X parcelas
        VNNUMPARCIMPOSTOS := VNNUMPARC;

        IF vrPARAMETROS.CON_LANCARSTAPENASPARC1 <> 'N' THEN

          IF vrPARAMETROS.CON_QTDEPARCELASIMPOSTOSFRETE < VNNUMPARC THEN
            VNNUMPARCIMPOSTOS := vrPARAMETROS.CON_QTDEPARCELASIMPOSTOSFRETE;
          ELSE
            VNNUMPARCIMPOSTOS := VNNUMPARC;
          END IF;

        ELSE
          VNNUMPARCIMPOSTOS := VNNUMPARC;
        END IF;

        -- Parcelas A
        vn_total_financ := ((vntotalger - vnvlfretea) - (vntotalipi) -
                           (vnicmsretido) - (vntotalvldifaliquotas));

        if ROW_PCNFSAID.CONDVENDA = 14 then
          vn_total_financ := vn_total_financ  - (VNTOTAL_TV1BNF);
        end if;

        /*
         Autor: Deyvid Costa.
         Solicitação: 1812.087947.2016.
         Data: 11/08/2016
         Descrição: Validando se a parcela ficou negativa ao retirar os impostos, caso sim, o percuntal dos
                    impostos são recalculados de acordo com o crédito concedido
                    Cenário ex.: Crédito = 100;
                                 Valor total financeiro = 100,50; (Valor da nota fiscal)
                                 ST = 10
                                 IPI = 10
        */
        IF (vn_total_financ < 0) and (vncredito > 0) and (vntotalger > 0) THEN
          DECLARE
            VNPERCENTUALCRED NUMBER;
          BEGIN
            -- identificando o percentual de credito sobre o valor total
            VNPERCENTUALCRED := (round((vncredito * 100) / vntotalger_ant,
                                       2) / 100);

            -- Aplicando o mesmo percentual nos impostos
            vnicmsretido    := ROUND(vnicmsretido -
                                     (vnicmsretido * VNPERCENTUALCRED),
                                     2);
            vntotalipi      := ROUND(vntotalipi -
                                     (vntotalipi / VNPERCENTUALCRED),
                                     2);
            vn_total_financ := ((vntotalger - vnvlfretea) - (vntotalipi) -
                               (vnicmsretido) - (vntotalvldifaliquotas));
          END;
        END IF;
        /* FIM - 1812.087947.2016 */

        vnvalorparccont := TRUNC((vn_total_financ / vnnumparc), 2);

        vndifercont := (vn_total_financ - (vnvalorparccont * vnnumparc));

        vnvalorparcipi := 0;
        vndiferipi     := 0;
        vnvalorparcst  := 0;
        vndiferst      := 0;

        vnvalorparcipi := TRUNC((vntotalipi / vnnumparcimpostos), 2);

        vndiferipi := (vntotalipi - (vnvalorparcipi * vnnumparcimpostos));

        --Tarefa: 204977
        IF NVL(vsutilizaprocessogeracaocpst, 'N') <> 'N' THEN

          vnvalorparcst := vnicmsretido;

          vndiferst := 0;

        ELSE

          vnvalorparcst := TRUNC((vnicmsretido / vnnumparcimpostos), 2);

          vndiferst := TRUNC((vnicmsretido -
                             (vnvalorparcst * vnnumparcimpostos)),
                             2);

        END IF;

        --Fim tarefa 124971

        -- Tarefa 98736 (Dif. de aliquotas)
        vnvalorparcdifaliquotas := TRUNC((vntotalvldifaliquotas /
                                         vnnumparcimpostos),
                                         2);
        /*
        Comentado a partir da tarefa: 1343.052663.2014 -
        Estava influenciando erroneamente na redução da primeira parcela
        vndiferdifaliquotas     := (vntotalvldifaliquotas -
                                   (vntotalvldifaliquotas *
                                   vnnumparcimpostos));  */
        -- Tarefa 60308: Tratamento de rateio de frete em X parcelas
        vnnumparcfrete := vnnumparc;

        IF vrPARAMETROS.CON_LANCARFRETEXPARCELAS = 'S' THEN
          IF vrPARAMETROS.CON_QTDEPARCELASIMPOSTOSFRETE < VNNUMPARC THEN
            vnnumparcfrete := NVL(vrPARAMETROS.CON_QTDEPARCELASIMPOSTOSFRETE,
                                  1);
          END IF;
        ELSE
          VNNUMPARCFRETE := VNNUMPARC;
        END IF;

        --Tarefa 142487
        IF vnnumparcfrete = 0 THEN
          vnnumparcfrete := 1;
        END IF;

        vnvalorparcfretea := TRUNC((vnvlfretea / vnnumparcfrete), 2);
        vndiferfretea     := (vnvlfretea -
                             (vnvalorparcfretea * vnnumparcfrete));
        vnvalorparcfreteb := TRUNC((vnvlfreteb / vnnumparcfrete), 2);
        vndiferfreteb     := (vnvlfreteb -
                             (vnvalorparcfreteb * vnnumparcfrete));

        vnvalorparc1     := vnvalorparc + vndifer;
        vnvalorparccont1 := vnvalorparccont + vndifercont;
        -- Voltar variavel para valor original
        vntotalger := vntotalger + (vnjuros * vnnumparccob);

        -- Tarefa 55008 (Não gerar PCPREST somente do boleto, quando houver desc. por crédito do cliente)
        IF vntotalger = (vnjuros * vnnumparccob) THEN
          vntotalger := 0;
        END IF;

        -- Tarefa 39481
        -- Retornando prazo Adional (PCPRAZOADICIONAL)
        validarpcprazoadicional(pddataemicao,
                                vnnumregiao_praca,
                                cab.codcli,
                                cab.codusur,
                                cab.origemped,
                                cab.codplpag,
                                vntotalger,
                                vdcodcidade,
                                cab.codfilial,
                                pvc2menssagen,
                                vnnumdias_prazoadicional);

        IF pvc2menssagen <> 'OK' THEN

          FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
          FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
          FATURAMENTO.vrPARAMETROS.MSG    := pvc2menssagen;

          FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                (CASE WHEN
                                 FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN
                                 DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                 TO_CHAR($$PLSQL_LINE) END));
          RAISE ERRO_GERAL;
          --            RAISE error_dados_carregamento;
        END IF;

        -- Fim tarefa 39481

        -- Calcula a Data de Vencimento ...
        IF vrPARAMETROS.CON_USAPRZADICIONALVENDA = 'N' THEN
          vnprazoadicional_client  := 0;
          vnnumdias_prazoadicional := 0;
        END IF;

        vnprazoadicional_cli    := vnprazoadicional_client +
                                   vnnumdias_prazoadicional;
        vnprazoadicional1       := vnprazoadicional_client +
                                   vnnumdias_prazoadicional;
        vnprazoadicional_client := vnprazoadicional_client +
                                   vnnumdias_prazoadicional;
        IF (cab.usaprazoadicionalpcclient = 'N') then
          vnprazoadicional_client  := 0;
          vnnumdias_prazoadicional := 0;
          vnprazoadicional_cli     := 0;
          vnprazoadicional1        := 0;
        END IF;


        IF vrPARAMETROS.CON_DIVIDEPRZADICIONALPARCELAS = 'S' THEN
          IF (TRUNC(vnprazoadicional_client / vnnumparc) * vnnumparc) <>
             vnprazoadicional_client THEN
            vnprazoadicional1 := TRUNC(vnprazoadicional_client / vnnumparc) +
                                 (vnprazoadicional_client -
                                  (TRUNC(vnprazoadicional_client /
                                         vnnumparc) * vnnumparc));
          ELSE
            vnprazoadicional1 := TRUNC(vnprazoadicional_client / vnnumparc);
          END IF;

          vnprazoadicional_client := TRUNC(vnprazoadicional_client /
                                           vnnumparc);
        END IF;

        vnprazomedio := 0;

        -- Calcula datas de vencimento
        IF vsformaparcelamento_plpag = 'C' THEN
          if (vddtvenc1 IS  NULL) THEN

            IF vddtvenc1 IS NULL THEN
              vddtvenc1    := pddataentregaproduto + vnprazoadicional1 +
                              NVL(vnprazo1, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional1 +
                              NVL(vnprazo1, 0);

            END IF;

            IF ((vddtvenc2 IS NULL) AND (NVL(vnprazo2, 0) > 0)) THEN
              vddtvenc2    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo2, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo2, 0);
            END IF;

            IF ((vddtvenc3 IS NULL) AND (NVL(vnprazo3, 0) > 0)) THEN
              vddtvenc3    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo3, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo3, 0);
            END IF;

            IF ((vddtvenc4 IS NULL) AND (NVL(vnprazo4, 0) > 0)) THEN
              vddtvenc4    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo4, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo4, 0);
            END IF;

            IF ((vddtvenc5 IS NULL) AND (NVL(vnprazo5, 0) > 0)) THEN
              vddtvenc5    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo5, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo5, 0);
            END IF;

            IF ((vddtvenc6 IS NULL) AND (NVL(vnprazo6, 0) > 0)) THEN
              vddtvenc6    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo6, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo6, 0);
            END IF;

            IF ((vddtvenc7 IS NULL) AND (NVL(vnprazo7, 0) > 0)) THEN
              vddtvenc7    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo7, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo7, 0);
            END IF;

            IF ((vddtvenc8 IS NULL) AND (NVL(vnprazo8, 0) > 0)) THEN
              vddtvenc8    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo8, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo8, 0);
            END IF;

            IF ((vddtvenc9 IS NULL) AND (NVL(vnprazo9, 0) > 0)) THEN
              vddtvenc9    := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo9, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo9, 0);
            END IF;

            IF ((vddtvenc10 IS NULL) AND (NVL(vnprazo10, 0) > 0)) THEN
              vddtvenc10   := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo10, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo10, 0);
            END IF;

            IF ((vddtvenc11 IS NULL) AND (NVL(vnprazo11, 0) > 0)) THEN
              vddtvenc11   := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo11, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo11, 0);
            END IF;

            IF ((vddtvenc12 IS NULL) AND (NVL(vnprazo12, 0) > 0)) THEN
              vddtvenc12   := pddataentregaproduto + vnprazoadicional_client +
                              NVL(vnprazo12, 0);
              vnprazomedio := vnprazomedio + vnprazoadicional_client +
                              NVL(vnprazo12, 0);
            END IF;

          else

            IF vddtvenc1 IS NOT NULL THEN
              vddtvenc1    := vddtvenc1 + vnprazoadicional1;
              vnprazomedio := vnprazomedio + vnprazoadicional1;
            END IF;

            IF ((vddtvenc2 IS NOT NULL) AND (NVL(vnprazo2, 0) > 0)) THEN
              vddtvenc2    := vddtvenc2 + vnprazoadicional_client;
              vnprazomedio := vnprazomedio + vnprazoadicional_client;
            END IF;

            IF ((vddtvenc3 IS NOT NULL) AND (NVL(vnprazo3, 0) > 0)) THEN
              vddtvenc3    := vddtvenc2 + vnprazoadicional_client;
              vnprazomedio := vnprazomedio + vnprazoadicional_client ;
            END IF;
          end if;
        ELSE
          -- FORMAPARCELAMENTO = 'M' -- Tarefa 33753
          -- vnprazomedio            := 30;
          vnprazoadicional_client := 0;
        END IF;

        vni                    := 1;

        IF NOT (VN_NUMPEDPGTOPEDIDO > 0) THEN -- HIS.00127.2017
          vnprest                := 1;
        END IF;

        vnprestb               := 'A';
        vbnaogerarpcprest_cred := FALSE;
        -- Tarefa 64571
        vsbloqdesdemitentedif_prest := 'N';

        IF (cab.usarateiocomissaooperador = 'S') AND
           (vsbloqdesdemitentedif_filial = 'N') THEN
          vsbloqdesdemitentedif_prest := 'N';
        ELSE
          IF (cab.usarateiocomissaooperador = 'S') AND
             (vsbloqdesdemitentedif_filial = 'S') THEN
            vsbloqdesdemitentedif_prest := 'S';
          END IF;
        END IF;

        vscodcob_pedc  := cab.codcob;
        vscodcob_prest := vscodcob_pedc;

        --Tarefa: 197968
        IF (vrPARAMETROS.FIL_ALTERARCOBBKCHFATURAMENTO = 'S') AND
           (vsboleto = 'S') --Tarefa: 204269
         THEN

          CASE vrPARAMETROS.CON_TIPOVLMINVENDABK
            WHEN 'VP' THEN
              --IF vnvalorparc < vsvlminvendabk THEN
              IF ((VN_VLTOT_PEDIDOS_VINC - vncredito) / vnnumparc) < vsvlminvendabk THEN
                --Tarefa: 202605

                IF (cab.codcob <> 'BNF') AND (cab.codcob <> 'BNFT') THEN

                  vscodcob_pedc := 'CH';
                  --Tarefa: 205164
                  UPDATE PCNFSAID
                     SET CODCOB = vscodcob_pedc
                   WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

                END IF;

                IF vnjuros > 0 THEN

                  vnvltarifa := 0;
                  --vnvloutrasdesp := vnvloutrasdesp - G_VNTXBOLETO;
                  vntotalger := vntotalger - G_VNTXBOLETO;
                  --vnjuros        := 0;
                  vsboleto := 'N';

                  --Tarefa: 205164
                  IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S' THEN
                    vnvloutrasdesp := vnvloutrasdesp - G_VNTXBOLETO;
                  END IF;

                END IF;

              END IF;
            WHEN 'VT' THEN
              --IF vntotalger < vsvlminvendabk THEN
              IF (VN_VLTOT_PEDIDOS_VINC - vncredito) < vsvlminvendabk THEN
                --Tarefa: 202605

                IF (cab.codcob <> 'BNF') AND (cab.codcob <> 'BNFT') THEN

                  vscodcob_pedc := 'CH';
                  --Tarefa: 205164
                  UPDATE PCNFSAID
                     SET CODCOB = vscodcob_pedc
                   WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

                END IF;

                IF vnjuros > 0 THEN

                  vnvltarifa := 0;
                  --vnvloutrasdesp := vnvloutrasdesp - G_VNTXBOLETO;
                  vntotalger := vntotalger - G_VNTXBOLETO;
                  --vnjuros        := 0;
                  vsboleto := 'N';

                  --Tarefa: 205164
                  IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S' THEN
                    vnvloutrasdesp := vnvloutrasdesp - G_VNTXBOLETO;
                  END IF;

                END IF;
              END IF;

          END CASE;

          vscodcob_prest := vscodcob_pedc; --> Tarefa: 204140

        END IF;
        --Fim Tarefa: 197968

        -- 5113.014099.2016 - Define percentual de comissão para a parcela de entrada.
       /* vnPERCOM_ENTRADA := CASE
                              WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                               0
                              ELSE
                               ROUND(((VNCOMISSAO / ((VNTOTALGER - (NVL(VNJUROS, 0) *
                                     VNNUMPARCCOB)) +
                                     NVL(VNCREDITO, 0))) * 100),
                                     5)
                            END;
        vnPERCOM_ENTRADA2 := CASE
                               WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                0
                               ELSE
                                ROUND(((VNCOMISSAO2 /
                                      ((VNTOTALGER -
                                      (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                      NVL(VNCREDITO, 0))) * 100),
                                      5)
                             END;
        vnPERCOM_ENTRADA3 := CASE
                               WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                0
                               ELSE
                                ROUND(((VNCOMISSAO3 /
                                      ((VNTOTALGER -
                                      (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                      NVL(VNCREDITO, 0))) * 100),
                                      5)
                             END;
        vnPERCOM_ENTRADA4 := CASE
                               WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                0
                               ELSE
                                ROUND(((VNCOMISSAO4 /
                                      ((VNTOTALGER -
                                      (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                      NVL(VNCREDITO, 0))) * 100),
                                      5)
                             END;  */
        -- 5113.014099.2016 - Define percentual de comissão para a parcela de entrada.

        IF vscodcob_pedc = 'CH' THEN
          vnjuros := 0;
        END IF;

        -----------------------INICIALIZAR CONTAS A RECEBER---------------------------------

        oCONTASRECEBER := T_PC_CONTASRECEBER(FATURAMENTO.G_FUNC.MATRICULA);
        oCONTASRECEBER.LIMPAR;

        ------------------------------------------------------------------------------------

        --Tarefa 128631
        /*
        IF VSFORMAPARCELAMENTO_PLPAG = 'T' THEN

          IF NVL(VSCODCOB_PEDC,'X') = 'BK' THEN
            VSCODCOB_PEDC := PVNCODIGOCOBRANCA;
          END IF;

          SELECT NVL(PCPLPAG.DIASCARENCIA, 0),
                 NVL(PCPLPAG.DIAFIXO, 1),
                 NVL(PCPLPAG.NUMEROPARCELASDIAFIXO, 1)
            INTO PCARENCIA, DIAFIXO, PNUMPARCELAS
            FROM PCPLPAG
           WHERE PCPLPAG.CODPLPAG = pncodplpag;

          PRC_GERAPCPREST_DATAFIXA(CAB.CODPLPAG,
                                   VN_NUMTRANSVENDA,
                                   vnperdescfin_client, --perdesc
                                   FATURAMENTO.G_FUNC.NUMCAIXABALCAO, --NUMCHECKOUT
                                   vsbloqdesdemitentedif_prest,
                                   --BLOQDESDEMITENTEDIF
                                   vnjuros,
                                   vscodcob_pedc,
                                   pddataemicao);

        ELSE*/
        -- Fim tarefa 64571
    begin
      select coalesce(UTILIZARECALCULOSTCHECKOUT, 'N')
        into vsUTILIZARECALCULOSTCHECKOUT
        from pcfilial
       where codigo = ROW_PCNFSAID.CODFILIAL;
    exception
      when others then
        vsUTILIZARECALCULOSTCHECKOUT := 'N';
    end;

    if not (cab.origemped = 'A' and vsUTILIZARECALCULOSTCHECKOUT = 'S')then
      IF cab.geracp = 'N' THEN
            LOOP
              --HIS.00127.2017
              /****************************************************************************************
               * O exit abaixo é para validar se existe multiplas formas de pagamento e a cobrança    *
               * seja da geração de ST, neste caso, então, não entrará na geração de prestação normal *
               * para a parcela referente ao ST, que foi feita automaticamnete pela rotina 316.       *
               ****************************************************************************************/
              EXIT WHEN (NVL(P_FORMAPGTOPEDIDO.CODCOB, 'X') = vrPARAMETROS.CODCOBST AND
                         (NVL(vsutilizaprocessogeracaocpst, 'N') = 'S')) or
                         (VN_NUMPEDPGTOPEDIDO > 0) AND
                         (NVL(P_FORMAPGTOPEDIDO.CODCOB, 'BNF') = 'BNF') AND
                         (vnTotal_TV1Bnf >= 0);


              vnindicea := 1;
              vnindiceb := 0;
              -- Taxa de boleto sempre vai em apenas 1 dos tipos de prest
              vnindatx := 1;
              vnindbtx := 0;

              -- Casos sem boleto
              IF (vsboleto <> 'S') AND (NVL(cab.codcob, ' ') <> 'BK') THEN
                vnindatx := 0;
                vnindbtx := 0;
              END IF;

              -- Tratamento da parcela 1
              vnindatx1 := vnindatx;
              vnindbtx1 := vnindbtx;

              /*IF vrPARAMETROS.CON_COBRARVLTARIFAPARC1 <> 'S' THEN
                vnindatx1 := 0;
                vnindbtx1 := 0;
              END IF;*/

              vddatavencimento := pddataentregaproduto;
              vddatavencaux    := pddataentregaproduto + vnnumdiascarencia;

              --Verifica se tem registro na PCDIASPLPAGCLI desse cliente
              select count(1)
                into vnPCDIASPLPAGCLI
                FROM PCDIASPLPAGCLI
               WHERE CODCLI =  CAB.CODCLI;

--VEN-1888 - Fernando.Carmo: Supplicard deve ser tratado com a forma de pagamento normal.
--              IF vsCobSupplierCard = 'N' then
                IF (vsformaparcelamento_plpag = 'C') THEN
                  IF vni = 1 THEN
                    vddatavencimento := vddtvenc1;
                  ELSIF vni = 2 THEN
                    vddatavencimento := vddtvenc2;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140

                  ELSIF vni = 3 THEN
                    vddatavencimento := vddtvenc3;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140

                  ELSIF vni = 4 THEN
                    vddatavencimento := vddtvenc4;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 5 THEN
                    vddatavencimento := vddtvenc5;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 6 THEN
                    vddatavencimento := vddtvenc6;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 7 THEN
                    vddatavencimento := vddtvenc7;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 8 THEN
                    vddatavencimento := vddtvenc8;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 9 THEN
                    vddatavencimento := vddtvenc9;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 10 THEN
                    vddatavencimento := vddtvenc10;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 11 THEN
                    vddatavencimento := vddtvenc11;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  ELSIF vni = 12 THEN
                    vddatavencimento := vddtvenc12;
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                  END IF;
                ELSIF (vsformaparcelamento_plpag = 'M') THEN
                  -- Tarefa 33753
                  IF (vni = 1) THEN
                    vddatavencimento := vddatavencaux + vnprazoadicional1;

                    -- Fim tarefa 103223
                  ELSE
                    vscodcob_prest   := vscodcob_pedc; --> Tarefa: 204140
                    vddatavencimento := ADD_MONTHS(vddatavencaux +
                                                   vnprazoadicional_client,
                                                   vni - 1);
                  END IF;
                ELSIF (vsformaparcelamento_plpag = 'F') -- Tarefa 111469
                 THEN
                  BEGIN
                    SELECT tab1.numdias
                      INTO vnnumdias_plpagparcelas
                      FROM (SELECT ROWNUM parcela, tab.numdias
                              FROM (SELECT numdias
                                      FROM pcplpagparcelas
                                     WHERE codplpag = cab.codplpag --Tarefa: 178929
                                     ORDER BY numdias) tab) tab1
                     WHERE tab1.parcela = vni;
                  EXCEPTION
                    WHEN OTHERS THEN
                      vnnumdias_plpagparcelas := 0;
                  END;

                  IF vddatavenc_anterior IS NULL THEN
                    vddatavenc_anterior := pddataentregaproduto;
                  END IF;

                  IF vni = 1 THEN
                    vddatavencimento := vddatavenc_anterior +
                                        vnprazoadicional1 +
                                        vnnumdias_plpagparcelas;
                  ELSE
                    vddatavencimento := vddatavenc_anterior +
                                        vnprazoadicional_client +
                                        vnnumdias_plpagparcelas;
                  END IF;
                ELSIF vsformaparcelamento_plpag = 'V' THEN
                  -- Tarefa 148009
                  BEGIN
                    SELECT dtvenc + DECODE(vni,
                                           1,
                                           vnprazoadicional1,
                                           vnprazoadicional_client) -- Pablo 20/12/2011
                      INTO vddatavencimento
                      FROM pcpedcvcto
                     WHERE numped = cab.numped
                       AND numparcela = vni;
                  EXCEPTION
                    WHEN OTHERS THEN
                      vddatavencimento := pddataentregaproduto +
                                          vnprazoadicional_client;
                  END;
                END IF;

                IF (vsformaparcelamento_plpag = 'T') OR (vnPCDIASPLPAGCLI > 0)  THEN
                  --O processo abaixo deve ser executado APENAS UMA VEZ!!!
                  SELECT NVL(PCPLPAG.DIASCARENCIA, 0),
                         NVL(PCPLPAG.DIAFIXO, 0),
                         NVL(PCPLPAG.NUMEROPARCELASDIAFIXO, 1)
                     INTO vnCARENCIA,
                         vnDIAFIXO,
                         vnNUMPARCELAS
                    FROM PCPLPAG
                   WHERE PCPLPAG.CODPLPAG = CAB.CODPLPAG;


                  IF(vsformaparcelamento_plpag = 'T')THEN
                    IF (vni = 1) THEN
                      vnnumparc := vnNUMPARCELAS;

                      SELECT COUNT(1) INTO VNTOTDIASPLPAGCLI FROM PCDIASPLPAGCLI WHERE CODCLI = CAB.CODCLI;

                       IF VNTOTDIASPLPAGCLI > 0 THEN
                        CARREGAR_DTVENCIMENTO(pddataemicao,
                                              vnCARENCIA,
                                              vnDIAFIXO,
                                              vnnumparc,
                                              CAB.CODCLI);
                       ELSE
                         CARREGAR_DTVENCIMENTO(pddataemicao,
                                              vnCARENCIA,
                                              vnDIAFIXO,
                                              vnnumparc);
                       END IF;
                    END IF;
                    vddatavencimento := G_DATAS(VNI).DTVENCIMENTO;
                  ELSE
                    IF (vni = 1) THEN
                      VNCARENCIA :=  vddatavencimento - TRUNC(SYSDATE);
                    elsif(vni > 1)then
                      VNCARENCIA := (G_DATAS(1).DTVENCIMENTO - TRUNC(SYSDATE)) +
                                    ((vddatavencimento - TRUNC(SYSDATE)) - (G_DATAS(1).DTVENCIMENTO - TRUNC(SYSDATE)));
                      --(G_DATAS(1).DTVENCIMENTO - TRUNC(SYSDATE))+(vddatavencimento - TRUNC(SYSDATE));
                    end if;
                       SELECT COUNT(1) INTO VNTOTDIASPLPAGCLI FROM PCDIASPLPAGCLI WHERE CODCLI = CAB.CODCLI;
                       IF VNTOTDIASPLPAGCLI > 0 THEN
                        CARREGAR_DTVENCIMENTO(pddataemicao,
                                              vnCARENCIA,
                                              vnDIAFIXO,
                                              vnnumparc,
                                              CAB.CODCLI);
                       ELSE
                         CARREGAR_DTVENCIMENTO(pddataemicao,
                                              vnCARENCIA,
                                              vnDIAFIXO,
                                              vnnumparc);
                       END IF;
                     vddatavencimento := G_DATAS(1).DTVENCIMENTO;
                  END IF;
                END IF;
--              ELSE
                /*
                Autor      : Rafael Braga
                Solicitação: HIS.00023.2017
                Data       : 02/02/2017
                Descrição  : Tratamento da data de vencimento para pagamento via suppliercard.
                */
--VEN-1888 - Fernando.Carmo: Supplicard deve ser tratado com a forma de pagamento normal.
--                vddatavencimento := trunc(sysdate) +
--                                    (vni * vnIntervaloParcelasSupp);
--              END IF;

              IF --(vni = 1) AND (vnvlentrada = 0) AND
               (cab.restricaotransp = 'N') AND (cab.numpedorigem IS NULL) AND
               ((vsboleto = 'S') OR (cab.codcob = 'BK')) THEN

                IF ((vddatavencimento - pddataentregaproduto) <
                   NVL(vrPARAMETROS.CON_NUMDIASMINVENDABK, 0)) AND
                   (cab.condvenda NOT IN (5, 11)) THEN
                  IF (vddatavencimento = pddataentregaproduto) -- Tarefa 74287
                   THEN
                    -- Tarefa 74287
                    SELECT COUNT(*)
                      INTO vicontador
                      FROM pccob
                     WHERE codcob = 'DH';

                    IF vicontador = 0 THEN
                      INSERT INTO pccob
                        (codcob, cobranca, pagcomissao)
                      VALUES
                        ('DH', 'DINHEIRO EM TRANSITO', 'S');
                    END IF;

                    IF (cab.codcob <> 'BNF') AND (cab.codcob <> 'BNFT') THEN
                      vscodcob_pedc  := 'DH';
                      vscodcob_prest := 'DH';
                    END IF;

                    IF vnjuros > 0 THEN

                      IF (vnvltarifa - (G_VNTXBOLETO / vnnumparccob)) >= 0 THEN
                        vnvltarifa := vnvltarifa -
                                      (G_VNTXBOLETO / vnnumparccob); -- Retirando um Tarifa Boleto do Total de Tarifas
                      END IF;

                      IF (vnvloutrasdesp - (G_VNTXBOLETO / vnnumparccob)) >= 0 THEN
                        vnvloutrasdesp := vnvloutrasdesp -
                                          (G_VNTXBOLETO / vnnumparccob);
                        vntotalger     := vntotalger -
                                          (G_VNTXBOLETO / vnnumparccob);
                      ELSIF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'N' THEN
                        --4001.098975.2016
                        vntotalger := vntotalger -
                                      (G_VNTXBOLETO / vnnumparccob);
                      END IF;
                      --vnjuros         := 0; --Tarefa: 174911: Zerando valor txboleto
                      vbdescboletoparc1 := TRUE; --Tarefa 74287
                    END IF;
                  ELSE
                    IF (vddatavencimento - TRUNC(SYSDATE) <
                       vrPARAMETROS.CON_NUMDIASMINVENDABK) THEN

                      IF (cab.codcob <> 'BNF') AND (cab.codcob <> 'BNFT') THEN
                        vscodcob_pedc  := 'DH';
                        vscodcob_prest := 'DH';
                      END IF;

                      IF vnjuros > 0 THEN
                        IF (vnvltarifa - (G_VNTXBOLETO / vnnumparccob)) >= 0 THEN
                          vnvltarifa := vnvltarifa -
                                        (G_VNTXBOLETO / vnnumparccob); -- Retirando um Tarifa Boleto do Total de Tarifas
                        END IF;

                        IF (vnvloutrasdesp - (G_VNTXBOLETO / vnnumparccob)) >= 0 THEN
                          vnvloutrasdesp := vnvloutrasdesp -
                                            (G_VNTXBOLETO / vnnumparccob);
                          vntotalger     := vntotalger -
                                            (G_VNTXBOLETO / vnnumparccob);
                        ELSIF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'N' THEN
                          --4001.098975.2016
                          vntotalger := vntotalger -
                                        (G_VNTXBOLETO / vnnumparccob);
                        END IF;

                        --vnjuros           := 0; --Tarefa: 174911: Zerando valor txboleto
                        vbdescboletoparc1 := TRUE;
                      END IF;

                    END IF;
                  END IF;
                  UPDATE PCNFSAID
                     SET CODCOB = vscodcob_pedc
                   WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

                  --VEN-813
                  IF (vscodcob_pedc = 'DH') THEN
                    FATURAMENTO.vrPARAMETROS.MSG := 'Altera codcob DH ...';
                    FATURAMENTO. GRAVARLOG(FATURAMENTO.vrPARAMETROS, TO_CHAR($$PLSQL_LINE));
                  END IF;

                END IF;
              END IF;

              -- Tarefa 131731
              IF vnperdescfin_client <> 0 THEN

                CASE PSTIPOTRATAMENTODESCFIN_CLIENT
                  WHEN 'DV' THEN
                    VDDTDESC_PREST := VDDATAVENCIMENTO -
                                      NVL(PNQTDDIASAPLICDESCFIN_CLIENT, 0);
                  WHEN 'AV' THEN
                    VDDTDESC_PREST := VDDATAVENCIMENTO;
                  WHEN 'DF' THEN
                    VDDTDESC_PREST := VDDATAVENCIMENTO +
                                      NVL(PNQTDDIASAPLICDESCFIN_CLIENT, 0); --PDDATAEMICAO + NVL(PNQTDDIASAPLICDESCFIN_CLIENT,0);
                  ELSE
                    -----------------------------------------------------------
                    --Autor: Rodrigo Santos
                    --Solicitação: 1366.084319.2014
                    --Data: 01/08/2014
                    --Descrição: Ajustado o "CASE" para retornar a situação ao usuário, facilitando a manutenção do problema.
                    -----------------------------------------------------------

                    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                    FATURAMENTO.vrPARAMETROS.MSG    := 'Tipo de tratamento para o desconto financeiro do cliente ' ||
                                                       cab.codcli ||
                                                       ' é inválido. Verifique na rotina 308. ';

                    pvc2menssagen := FATURAMENTO.vrPARAMETROS.MSG;

                    FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                          (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                           '%ORA-%' THEN
                                           DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                           TO_CHAR($$PLSQL_LINE) END));

                    VBPULOUPEDIDO := TRUE;
                    --  GOTO FIM_LOOP_CAB;
                END CASE;
                /*Tarefa: 211612
                IF PSTIPOTRATAMENTODESCFIN_CLIENT = 'DV' THEN
                  vddtdesc_prest := vddatavencimento;
                ELSIF pstipotratamentodescfin_client = 'AV' THEN
                  vddtdesc_prest := vddatavencimento -
                                    pnqtddiasaplicdescfin_client;
                END IF;*/
              END IF;
              -- Fim tarefa 131731

              IF (vscodcob_pedc NOT IN ('DH', 'CH')) THEN

                IF (vrPARAMETROS.CON_SOMATXBOLETO = 'S') OR
                   (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S') THEN

                  IF (vrPARAMETROS.CON_COBRARVLTARIFAPARC1 = 'N') AND
                     (vbcobrartarifabanc = TRUE) AND (vni = 1)/*(vnprest = 1)*/ THEN
                    vbcobrartarifabanc := FALSE;
                  ELSE
                    vbcobrartarifabanc := TRUE;
                  END IF;
                ELSE
                  vbcobrartarifabanc := FALSE;
                END IF;
              ELSE
                vbcobrartarifabanc := FALSE;
              END IF;

              IF vbcobrartarifabanc THEN
                vnjurosprest := vnjuros;
              ELSE
                vnjurosprest := 0;
              END IF;

              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Totalizando NF...';

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                    (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                     '%ORA-%' THEN
                                     DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                     TO_CHAR($$PLSQL_LINE) END));

              -- Atentar para qualquer alteracao no Insert do PCPREST pois existem dois INSERT
              IF (vnindicea > 0) AND
                 ((((VNVALORPARCCONT1 + VNVALORPARCIPI + VNVALORPARCST) > 0) AND
                 (VNI = 1)) OR (((VNVALORPARCCONT + VNVALORPARCIPI +
                 VNVALORPARCST) > 0) AND (VNI <> 1))) THEN

                --Gerar parcela comum
                FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                FATURAMENTO.vrPARAMETROS.MSG    := 'Definido valor de tarifa de boleto da prestação ' ||
                                                   --TO_CHAR(vnprest) ||
                                                   TO_CHAR(vni) ||
                                                   ' de R$' ||
                                                   TO_CHAR(ROUND(visomatarifapcprest *
                                                                 (vnjurosprest *
                                                                 vnindatx),
                                                                 2));

                FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                      (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                       '%ORA-%' THEN
                                       DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                       TO_CHAR($$PLSQL_LINE) END));

                IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'N' THEN
                  visomatarifanf := 0;
                ELSE
                  visomatarifanf := 1;
                END IF;

                -- Parâmetro "vrPARAMETROS.CON_SOMATARIFABANCDUPLIC" pode ser por filial ou geral
                IF (vrPARAMETROS.CON_SOMATXBOLETO = 'N') AND
                   (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'N') THEN
                  visomatarifapcprest := 0;
                ELSIF ((vrPARAMETROS.CON_SOMATXBOLETO = 'S') AND
                      (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC IN ('S'))) THEN
                  VISOMATARIFAPCPREST := 1;
                  VSSOMATXBOLETO      := 'N'; -- se o parâmetro "FIL_SOMATARIFABANCDUPLIC"  = "SIM", então, não deve gravar o campo "PCPREST.SOMATXBOLETO".
                ELSE
                  VISOMATARIFAPCPREST := 1;
                  VSSOMATXBOLETO      := vrPARAMETROS.CON_SOMATXBOLETO;
                END IF;

                --Caso seja plano de pagamento de data fixa (tipo 'T')
                --buscar datas de vencimento no vetor G_DATAS
                IF vsformaparcelamento_plpag = 'T' THEN
                  VDDATAVENCIMENTO := G_DATAS(VNI).DTVENCIMENTO;
                END IF;

                --Código abaixo verifica/altera a data de vencimento para o dia da semana disponivel para pagamento
                --Informado pela 302(PCDIASEMANAPAGCLI) caso não tenha cadastro permanece a data ja calculada anteriomente
                --HIS.00334.2016
                VDDATAVENCIMENTO := FUNC_DIASEMANAVALIDO(CAB.CODFILIAL,
                                                         VDDATAVENCIMENTO,
                                                         1,
                                                         CAB.CODCLI);

                --Valida se o vencimento é valido de acordo com o dia maximo para pagamento do cliente.
                --HIS.00388.2016
                BEGIN
                  SELECT Count(*)
                    into vndialimitvecimento
                    FROM DUAL
                   WHERE EXTRACT(DAY FROM SYSDATE) >
                         (SELECT PCCLIENT.DIALIMITFATU
                            FROM PCCLIENT
                           WHERE CODCLI = CAB.CODCLI);

                  SELECT NVL(PCCLIENT.DIALIMITFATU, 0)
                    into vdDIALIMITFATU
                    FROM PCCLIENT
                   WHERE CODCLI = CAB.CODCLI;

                  IF (vndialimitvecimento > 0) then

                    RAISE ERRO_VALIDARDATAVENCIMENTO;
                  END IF;

                EXCEPTION
                  WHEN ERRO_VALIDARDATAVENCIMENTO THEN
                    pvc2menssagen := 'Data de faturamento do cliente invalida. Verificar o cadastro do cliente!' ||
                                     ' Cod.Cliente: ' || TO_CHAR(CAB.CODCLI) ||
                                     ' Vencimento: ' ||
                                     TO_CHAR(vdDIALIMITFATU);

                    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                    FATURAMENTO.vrPARAMETROS.MSG    := pvc2menssagen;
                    FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                          (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                           '%ORA-%' THEN
                                           DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                           TO_CHAR($$PLSQL_LINE) END));
                    VBPULOUPEDIDO := TRUE;
                    --GOTO FIM_LOOP_CAB;

                  WHEN OTHERS THEN
                    pvc2menssagen := 'Erro Ao verificar Data de faturamento do cliente.!' ||
                                     ' Cod.Cliente: ' || TO_CHAR(CAB.CODCLI) ||
                                     ' Data Limite: ' ||
                                     TO_CHAR(VDDATAVENCIMENTO) || CHR(13) ||
                                     SQLCODE || '-' || SQLERRM || CHR(13) ||
                                     DBMS_UTILITY.format_error_backtrace || -- Deyvid Costa - Retorna a linha que gerou a exceção
                                     CHR(13);

                    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                    FATURAMENTO.vrPARAMETROS.MSG    := pvc2menssagen;
                    FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                          (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                           '%ORA-%' THEN
                                           DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                           TO_CHAR($$PLSQL_LINE) END));
                    VBPULOUPEDIDO := TRUE;
                    --GOTO FIM_LOOP_CAB;
                END;
                -----------------------------PARCELA COMUM------------------------------------
                oCONTASRECEBER.LIMPAR;
                FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando PCPREST, parcela comum.';
                FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, $$PLSQL_LINE);

                oCONTASRECEBER.CODFILIAL := CAB.CODFILIAL; -- 01
                oCONTASRECEBER.CODCLI    := CAB.CODCLI; -- 02
                oCONTASRECEBER.CODUSUR   := CAB.CODUSUR; -- 03
                oCONTASRECEBER.CODUSUR2  := CAB.CODUSUR2; -- 37
                oCONTASRECEBER.CODUSUR3  := CAB.CODUSUR3; -- 38
                oCONTASRECEBER.CODUSUR4  := CAB.CODUSUR4; -- 39
                oCONTASRECEBER.PREST     := fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA);--VNPREST; -- 04
                oCONTASRECEBER.DUPLIC    := VN_NUMNOTA; -- 05

                ---------Valor da Prestação (MUITA ATENÇÃO AO ALTERAR ESTE PONTO!!!)----------

                IF VNI = 1 THEN
                  --> Se for primeira prest(prest = 1) receberá os centavos de diferença

                  --Valor da prest recebe seu valor gerencial juntamente com taxa de boleto casa haja.
                  oCONTASRECEBER.VALOR := (VNVALORPARCCONT1 * VNINDICEA) +
                                          (VISOMATARIFANF *
                                          (VNJUROSPREST * VNINDATX1));

                  --Agregando valor de frete
                  IF (VNI <= VNNUMPARCFRETE) THEN

                    oCONTASRECEBER.VALOR := oCONTASRECEBER.VALOR +
                                            (VNVALORPARCFRETEA +
                                            VNDIFERFRETEA);

                  END IF;

                  --Agregando valor de impostos
                  IF (VNI <= VNNUMPARCIMPOSTOS) THEN

                    oCONTASRECEBER.VALOR := oCONTASRECEBER.VALOR +
                                            (VNVALORPARCIPI + VNDIFERIPI +
                                            VNVALORPARCDIFALIQUOTAS); /*+
                                                                                                                VNDIFERDIFALIQUOTAS);*/
                    --O valor de ST só é somado a prest comum se
                    --não estiver sendo gerado separadamente.

                    IF NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'N' THEN
                      oCONTASRECEBER.VALOR := oCONTASRECEBER.VALOR +
                                              (VNVALORPARCST + VNDIFERST);
                    END IF;

                  END IF;
                ELSE

                  --Valor da prest recebe seu valor gerencial juntamente com taxa de boleto casa haja.
                  oCONTASRECEBER.VALOR := (VNVALORPARCCONT * VNINDICEA) +
                                          (VISOMATARIFANF *
                                          (VNJUROSPREST * VNINDATX));

                  --Agregando valor de frete
                  IF (VNI <= VNNUMPARCFRETE) THEN

                    oCONTASRECEBER.VALOR := oCONTASRECEBER.VALOR +
                                            VNVALORPARCFRETEA;

                  END IF;

                  --Agregando valor de impostos
                  IF (VNI <= VNNUMPARCIMPOSTOS) THEN

                    oCONTASRECEBER.VALOR := oCONTASRECEBER.VALOR +
                                            (VNVALORPARCIPI +
                                            VNVALORPARCDIFALIQUOTAS);
                    --O valor de ST só é somado a prest comum se
                    --não estiver sendo gerado separadamente.
                    IF NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'N' THEN
                      oCONTASRECEBER.VALOR := oCONTASRECEBER.VALOR +
                                              VNVALORPARCST;
                    END IF;

                  END IF;

                END IF;
                --------------------- Fim Valor da Prestação -----------------------

                FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                FATURAMENTO.vrPARAMETROS.MSG    := 'Valor da prestação: PCPREST.VALOR = ' ||
                                                   oCONTASRECEBER.VALOR;
                FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, $$PLSQL_LINE);

                IF (NVL(VSCODCOB_PEDC, 'X') = 'BK') AND
                   (NVL(VSCODCOB_PEDC, 'X') <> 'X') THEN

                  oCONTASRECEBER.CODCOB := PVNCODIGOCOBRANCA;

                ELSIF (NVL(VSCODCOB_PEDC, 'X') = 'X') THEN

                  FATURAMENTO.VRPARAMETROS.NUMPED := CAB.NUMPED;
                  FATURAMENTO.VRPARAMETROS.DATA   := SYSDATE;
                  FATURAMENTO.VRPARAMETROS.MSG    := 'Código de cobrança inválido!' ||
                                                     ' CODCOB: ' ||
                                                     TO_CHAR(NVL(VSCODCOB_PEDC,
                                                                 'X'));

                  FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                        (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                         '%ORA-%' THEN
                                         DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                         TO_CHAR($$PLSQL_LINE) END));

                  RAISE ERRO_CONTASARECEBER;

                ELSE
                  oCONTASRECEBER.CODCOB := NVL(VSCODCOB_PEDC, 'X');
                END IF; -- 07

                oCONTASRECEBER.DTVENC        := VDDATAVENCIMENTO; -- 08
                oCONTASRECEBER.DTVENCORIG    := VDDATAVENCIMENTO; -- 09
                oCONTASRECEBER.DTEMISSAO     := PDDATAEMICAO; -- 10
                oCONTASRECEBER.OPERACAO      := 'N'; -- 11
                oCONTASRECEBER.BOLETO        := '1'; -- 12
                oCONTASRECEBER.STATUS        := 'A'; -- 13
                oCONTASRECEBER.NUMCAR        := CAB.NUMCAR; -- 14
                oCONTASRECEBER.CODSUPERVISOR := VNCODSUPERVISOR_USUARI; -- 15
                oCONTASRECEBER.NUMTRANSVENDA := VN_NUMTRANSVENDA; -- 16
                oCONTASRECEBER.VLTXBOLETO    := (VNJUROSPREST *
                                                VISOMATARIFAPCPREST); -- 17

              ------------------------- Valor de Comissão -----------------------------
              DECLARE
              -- VARIÁVEIS LOCAL
                VNPERCPARCMULT    NUMBER := 0;
                VNTOTALCOBPED     NUMBER := 0;
                VNVALORPARCSTLIQ  NUMBER := 0;
                VNVALORPARCIPILIQ NUMBER := 0;
                vndiferstLIQ      NUMBER := 0;
                VNDIFERIPILIQ     NUMBER := 0;

              BEGIN
                VNVALORPARCSTLIQ  := VNVALORPARCST; -- VARIÁVEL GLOBAL
                VNVALORPARCIPILIQ := VNVALORPARCIPI; -- VARIÁVEL GLOBAL

                IF P_FORMAPGTOPEDIDO.VALOR > 0 THEN
                  FOR DADOS IN (SELECT C.*
                                  FROM PCFORMAPGTOPEDIDO C
                                 WHERE C.NUMPED = CAB.NUMPED
                                   AND C.CODCOB <> 'BNF') LOOP
                    VNTOTALCOBPED := VNTOTALCOBPED + NVL(DADOS.VALOR, 0) -
                                     NVL(DADOS.VLRTROCO, 0) -
                                     NVL(DADOS.VLENTRADA, 0);
                    IF (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S') THEN
                      VNTOTALCOBPED := VNTOTALCOBPED +
                                       FATURAMENTO.VALORTARIFA(CAB.NUMPED,
                                                               CAB.VLATEND,
                                                               DADOS);
                    END IF;
                  END LOOP;

                  VNPERCPARCMULT    := (P_FORMAPGTOPEDIDO.VALOR / VNTOTALCOBPED) * 100; --ENCONTRANDO O PERCENTUAL POR PESO(PARCELAS)

                  VNVALORPARCSTLIQ  := ROUND((VNVALORPARCSTLIQ * VNPERCPARCMULT) / 100,
                                             2);
                  VNVALORPARCIPILIQ := ROUND((VNVALORPARCIPILIQ *
                                             VNPERCPARCMULT) / 100,
                                             2);
                END IF;

                IF vrPARAMETROS.CON_ABATERIMPOSTOSCOMISSAORCA = 'S' THEN
                  IF VNI = 1 THEN
                    CASE vrPARAMETROS.IMPOSTOCOMISSAORCA
                      WHEN 'A' THEN
                        oCONTASRECEBER.VALORLIQCOM := CASE
                                                        WHEN NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'S' THEN
                                                         ROUND(oCONTASRECEBER.VALOR - (VNVALORPARCIPILIQ + VNDIFERIPI),
                                                               2)
                                                        ELSE
                                                         ROUND(oCONTASRECEBER.VALOR - ((VNVALORPARCSTLIQ + VNDIFERST) +
                                                               (VNVALORPARCIPILIQ + VNDIFERIPI)),
                                                               2)
                                                      END;
                      WHEN 'I' THEN
                        oCONTASRECEBER.VALORLIQCOM := ROUND(oCONTASRECEBER.VALOR -
                                                            (VNVALORPARCIPILIQ +
                                                            VNDIFERIPI),
                                                            2);

                      WHEN 'S' THEN
                        oCONTASRECEBER.VALORLIQCOM := CASE
                                                        WHEN NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'S' THEN
                                                         ROUND(oCONTASRECEBER.VALOR, 2)
                                                        ELSE
                                                         ROUND(oCONTASRECEBER.VALOR - (VNVALORPARCSTLIQ + VNDIFERST), 2)
                                                      END;
                    END CASE;

                  ELSIF (VNI <= VNNUMPARCIMPOSTOS) THEN
                    CASE vrPARAMETROS.IMPOSTOCOMISSAORCA
                      WHEN 'A' THEN
                        oCONTASRECEBER.VALORLIQCOM := CASE
                                                        WHEN NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'S' THEN
                                                         ROUND(oCONTASRECEBER.VALOR - (VNVALORPARCIPILIQ), 2)
                                                        ELSE
                                                         ROUND(oCONTASRECEBER.VALOR -
                                                               ((VNVALORPARCSTLIQ) + (VNVALORPARCIPILIQ)),
                                                               2)
                                                      END;
                      WHEN 'I' THEN
                        oCONTASRECEBER.VALORLIQCOM := ROUND(oCONTASRECEBER.VALOR -
                                                            (VNVALORPARCIPILIQ),
                                                            2);

                      WHEN 'S' THEN
                        oCONTASRECEBER.VALORLIQCOM := CASE
                                                        WHEN NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'S' THEN
                                                         ROUND(oCONTASRECEBER.VALOR, 2)
                                                        ELSE
                                                         ROUND(oCONTASRECEBER.VALOR - (VNVALORPARCSTLIQ), 2)
                                                      END;
                    END CASE;

                  ELSE
                    oCONTASRECEBER.VALORLIQCOM := oCONTASRECEBER.VALOR;

                  END IF;

                ELSE
                  oCONTASRECEBER.VALORLIQCOM := oCONTASRECEBER.VALOR;
                END IF; -- 18
              END;
              ----------------------- Fim Valor de Comissão -----------------------------

                BEGIN
                  --Solicitação: 1592.000894.2013
                  SELECT NVL(PAGCOMISSAO, 'N')
                    INTO VSPAGCOMISSAO_COB
                    FROM PCCOB
                   WHERE CODCOB = oCONTASRECEBER.CODCOB;
                EXCEPTION
                  WHEN OTHERS THEN
                    FATURAMENTO.VRPARAMETROS.NUMPED := CAB.NUMPED;
                    FATURAMENTO.VRPARAMETROS.DATA   := SYSDATE;
                    FATURAMENTO.VRPARAMETROS.MSG    := 'Erro ao buscar dados da cobrança: ' ||
                                                       TO_CHAR(oCONTASRECEBER.CODCOB) ||
                                                       chr(13) ||
                                                       'Erro original: ' ||
                                                       SQLERRM;

                    FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                          (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                           '%ORA-%' THEN
                                           DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                           TO_CHAR($$PLSQL_LINE) END));

                    VBPULOUPEDIDO := TRUE;
                    -- GOTO FIM_LOOP_CAB;
                END;

                /*oCONTASRECEBER.PERCOM     := CASE VSPAGCOMISSAO_COB
                                               WHEN 'N' THEN
                                                 0
                                               ELSE
                                                 CASE
                                                   WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                                     0
                                                   ELSE
                                                     CASE vrparametros.ABATERDESCFINCOMISSAORCA
                                                       WHEN 'S' THEN
                                                         ROUND(((VNCOMISSAO / (((VNTOTALGER - G_DESCFIN.VLDESCTOT) - (NVL(VNJUROS,0) * VNNUMPARCCOB)) + NVL(VNCREDITO,0))) * 100), 5) --- 207547
                                                       WHEN 'N' THEN
                                                         ROUND(((VNCOMISSAO / ((VNTOTALGER - (NVL(VNJUROS,0) * VNNUMPARCCOB)) + NVL(VNCREDITO,0))) * 100), 5)
                                                     END
                                                 END
                                             END; -- 19

                oCONTASRECEBER.CODEPTO    := CAB.CODEPTO; -- 20
                oCONTASRECEBER.VALORORIG  := oCONTASRECEBER.VALOR; -- 21
                oCONTASRECEBER.CODCOBORIG := oCONTASRECEBER.CODCOB; -- 22*/

                -- --- 207547 Guilherme Freitas - Passei para cima. procurar o numero da tarefa acima.
                /*------------------------------ Desconto Financeiro ----------------------------------
                Tarefa: 209726
                Melhoria:
                      Possibilidade de apurar o desconto financeiro por Região, Cliente, Fornecedor,
                Departamento, Produto e/ou Filial. Para isso o valor total de desconto financeiro deve ser
                apurado separadamente, item a item, pois podem existir descontos diferentes entre eles.
                      Abaixo o valor total dos descontos é acumulado em "G_DESCFIN.VLDESCTOT", para que
                no fim possa ser rateado entre as parcelas do contas à receber, o valor de desconto para
                cada parcela é definido em "G_DESCFIN.VLDESCPARC", ainda existe "G_DESCFIN.VLDIFPARC" para
                ser agregado na primeira parcela. Quando a apuração ocorrer desta forma o percentual do
                desconto "G_DESCFIN.PERDESC", é calculado inversamente, ou seja, valor de desconto financeiro
                sobre o valor total gerencial.
                --------------------- APURAÇÃO DO VALOR DE DESCONTO FINANCEIRO -----------------------*/
                vnDESCFIN_TEMP       := 0;
                G_DESCFIN.PERDESC    := 0;
                G_DESCFIN.VLDESCTOT  := 0;
                G_DESCFIN.VLDESCPARC := 0;
                G_DESCFIN.VLDIFPARC  := 0;
                --G_DESCFIN.VSDESCPORITEM := 'N';

                if ROW_PCNFSAID.VLTOTGER > VNCREDITO  then
                   vPercvenpcre := ((ROW_PCNFSAID.VLTOTGER - VNCREDITO)*100)/ROW_PCNFSAID.VLTOTGER;
                else
                  vPercvenpcre := 100;
                end if;

                FOR ITEM_PED IN (SELECT P.CODFILIAL,
                                        P.CODCLI,
                                        I.CODPROD,
                                        I.QT,
                                        (I.PVENDA * vPercvenpcre /100) PVENDA,
                                        I.NUMPED,
                                        I.NUMSEQ,
                                        NVL(I.ST, 0) ST,
                                        NVL(I.VLIPI, 0) VLIPI,
                                        NVL(I.VLFECP,0) VLFECP
                                   FROM PCPEDC P, PCPEDI I
                                  WHERE P.NUMPED = I.NUMPED
                                  AND P.NUMPED = CAB.NUMPED
                                  AND NVL(I.BONIFIC, 'N') NOT IN('F')) LOOP

                  /*Alterada a chamada da função abaixo para passar o número
                  da região do pedido. A rotina pegava sempre a região da
                  PCPRACA vinculada e esse comportamento era problemático
                  quando o osuário utilizava tabela de preço por região
                  informados no cadastro do cliente, PCTABPRCLI.
                  lucas.lima | 2065.067449.2014 | 30/06/2014              */
                  vnDESCFIN_TEMP := NVL(F_BUSCARDESCFIN(ITEM_PED.CODPROD,
                                                        ITEM_PED.CODCLI,
                                                        ITEM_PED.CODFILIAL,
                                                        CAB.NUMREGIAO,
                                                        cab.usadescfinanceiro,
                                                        cab.origemped,
                                                        vnMAXDESC,
                                                        vnCODCAMP),
                                        0);

                  IF (NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'S') OR
                   (NVL(VSAGREGARVALORSTDESCFIN, 'N') = 'S') THEN

                    IF vrPARAMETROS.CON_ABATERIMPOSTOSDESCFIN = 'S' THEN
                      G_DESCFIN.VLDESCTOT := G_DESCFIN.VLDESCTOT +
                                             ROUND(((((ITEM_PED.PVENDA -
                                                   ITEM_PED.ST -
                                                   ITEM_PED.VLIPI -
                                                   ITEM_PED.VLFECP) *
                                                   ITEM_PED.QT) *
                                                   vnDESCFIN_TEMP) / 100),
                                                   6);
                    ELSE
                      G_DESCFIN.VLDESCTOT := G_DESCFIN.VLDESCTOT +
                                             ROUND(((((ITEM_PED.PVENDA -
                                                   ITEM_PED.ST -
                                                   ITEM_PED.VLFECP) *
                                                   ITEM_PED.QT) *
                                                   vnDESCFIN_TEMP) / 100),
                                                   6);
                  END IF;

                ELSE

                  IF vrPARAMETROS.CON_ABATERIMPOSTOSDESCFIN = 'S' THEN
                    G_DESCFIN.VLDESCTOT := G_DESCFIN.VLDESCTOT +
                                           ROUND(((((ITEM_PED.PVENDA -
                                                 ITEM_PED.ST -
                                                 ITEM_PED.VLIPI -
                                                 ITEM_PED.VLFECP) *
                                                 ITEM_PED.QT) *
                                                 vnDESCFIN_TEMP) / 100),
                                                 6);
                  ELSE
                    G_DESCFIN.VLDESCTOT := G_DESCFIN.VLDESCTOT +
                                           ROUND(((((ITEM_PED.PVENDA) *
                                                 ITEM_PED.QT) *
                                                 vnDESCFIN_TEMP) / 100),
                                                 6);
                  END IF;

                END IF;
                  IF (G_DESCFIN.VLDESCTOT > vnMAXDESC) AND (vnMAXDESC > 0) THEN
                    G_DESCFIN.VLDESCTOT := 0;
                    vnCODCAMP := 0;
                  ELSE
                    UPDATE PCMOVCOMPLE C
                       SET C.CODPOLITICADESC = vnCODCAMP
                     WHERE NUMTRANSITEM IN
                           (select m.numtransitem
                              from pcmov m
                             where m.numped = ITEM_PED.NUMPED
                               and m.codprod = ITEM_PED.Codprod
                               and m.numseq = ITEM_PED.Numseq);
                  END IF;
              END LOOP;

              -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
              retornarPERCOM(pnNUMTRANSVENDA   => VN_NUMTRANSVENDA,
                             vrPARAMETROS      => vrPARAMETROS,
                             VNNUMPARCCOB      => VNNUMPARCCOB,
                             PSCODCOB          => oCONTASRECEBER.CODCOB,
                             PSTIPOPARCELA     => 'COMUM',
                             OUTPERCOM         => oCONTASRECEBER.PERCOM,
                             OUTPERCOM2        => oCONTASRECEBER.PERCOM2,
                             OUTPERCOM3        => oCONTASRECEBER.PERCOM3,
                             OUTPERCOM4        => oCONTASRECEBER.PERCOM4,
                             OUTPERCOMLIQ      => oCONTASRECEBER.PERCOMLIQ);
              -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
              /*oCONTASRECEBER.PERCOM := CASE VSPAGCOMISSAO_COB
                                         WHEN 'N' THEN
                                          0
                                         ELSE
                                          CASE
                    IF vrPARAMETROS.CON_ABATERIMPOSTOSDESCFIN = 'S' THEN
                      G_DESCFIN.VLDESCTOT := G_DESCFIN.VLDESCTOT +
                                             ROUND(((((ITEM_PED.PVENDA -
                                                   ITEM_PED.ST -
                                                   ITEM_PED.VLIPI) *
                                                   ITEM_PED.QT) *
                                                   vnDESCFIN_TEMP) / 100),
                                                   6);
                    ELSE
                      G_DESCFIN.VLDESCTOT := G_DESCFIN.VLDESCTOT +
                                             ROUND(((((ITEM_PED.PVENDA) *
                                                   ITEM_PED.QT) *
                                                   vnDESCFIN_TEMP) / 100),
                                                   6);
                    END IF;

                  END IF;

                    IF G_DESCFIN.VLDESCTOT > vnMAXDESC THEN
                      G_DESCFIN.VLDESCTOT := 0;
                      vnCODCAMP := 0;
                    ELSE
                      UPDATE PCMOVCOMPLE C
                         SET C.CODPOLITICADESC = vnCODCAMP
                       WHERE NUMTRANSITEM IN
                             (select m.numtransitem
                                from pcmov m
                               where m.numped = ITEM_PED.NUMPED
                                 and m.codprod = ITEM_PED.Codprod
                                                      NVL(VNCREDITO, 0))) * 100),
                                                      5)
                                             END
                                          END
                                       END;*/ -- 19


                oCONTASRECEBER.CODEPTO    := CAB.CODEPTO; -- 20
                oCONTASRECEBER.VALORORIG  := oCONTASRECEBER.VALOR; -- 21
                oCONTASRECEBER.CODCOBORIG := oCONTASRECEBER.CODCOB; -- 22

                DECLARE
                -- VARIÁVEIS LOCAL
                VNPERCPARCMULT    NUMBER := 0;
                VNTOTALCOBPED     NUMBER := 0;
                BEGIN
                  G_DESCFIN.VLDESCPARC := TRUNC(((G_DESCFIN.VLDESCTOT) /
                                                vnnumparc),
                                                2);
                  G_DESCFIN.VLDIFPARC  := (G_DESCFIN.VLDESCTOT) -
                                          (G_DESCFIN.VLDESCPARC * vnnumparc);

                  IF P_FORMAPGTOPEDIDO.VALOR > 0 THEN

                    FOR DADOS IN (SELECT C.*
                                    FROM PCFORMAPGTOPEDIDO C
                                   WHERE C.NUMPED = CAB.NUMPED
                                     AND C.CODCOB <> 'BNF') LOOP
                      VNTOTALCOBPED := VNTOTALCOBPED + NVL(DADOS.VALOR, 0) -
                                       NVL(DADOS.VLRTROCO, 0) -
                                       NVL(DADOS.VLENTRADA, 0);

                      IF (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S') THEN
                        VNTOTALCOBPED := VNTOTALCOBPED +
                                         FATURAMENTO.VALORTARIFA(CAB.NUMPED,
                                                                 CAB.VLATEND,
                                                                 DADOS);
                      END IF;

                    END LOOP;

                    VNPERCPARCMULT := (P_FORMAPGTOPEDIDO.VALOR /
                                      VNTOTALCOBPED) * 100; --ENCONTRANDO O PERCENTUAL POR PESO(PARCELAS)


                    G_DESCFIN.VLDESCPARC  :=  TRUNC((((G_DESCFIN.VLDESCTOT * VNPERCPARCMULT) / 100) /
                                                vnnumparc),
                                                2);

                  END IF;

                  IF VNI = 1 THEN
                    G_DESCFIN.PERDESC := ROUND((((G_DESCFIN.VLDESCPARC +
                                               G_DESCFIN.VLDIFPARC) / (CASE
                                                 WHEN oCONTASRECEBER.VALOR = 0 THEN
                                                  1
                                                 ELSE
                                                  oCONTASRECEBER.VALOR
                                               END)) * 100),
                                               6);

                  ELSE
                    G_DESCFIN.PERDESC := ROUND(((G_DESCFIN.VLDESCPARC / (CASE
                                                 WHEN oCONTASRECEBER.VALOR = 0 THEN
                                                  1
                                                 ELSE
                                                  oCONTASRECEBER.VALOR
                                               END)) * 100),
                                               6);
                  END IF;

                END;

                /*4732.077152.2016 a variável VSDESCPORITEM usada antes nunca era alimentada*/
                IF cab.usadescfinanceiro = 'N' THEN
                  G_DESCFIN.PERDESC := VNPERDESCFIN_CLIENT;
                END IF;
                ------------------ FIM APURAÇÃO DO VALOR DE DESCONTO FINANCEIRO ------------------------
                --Fim Tarefa: 209726*/

                IF (NVL(G_DESCFIN.PERDESC, 0) <> NVL(VNPERDESCFIN_CLIENT, 0)) THEN
                  IF NVL(G_DESCFIN.PERDESC, 0) > 0 THEN
                    oCONTASRECEBER.PERDESC := G_DESCFIN.PERDESC;
                  ELSIF NVL(VNPERDESCFIN_CLIENT, 0) > 0 THEN
                    oCONTASRECEBER.PERDESC := VNPERDESCFIN_CLIENT;
                  ELSE
                    oCONTASRECEBER.PERDESC := 0;
                  END IF;
                ELSE
                  oCONTASRECEBER.PERDESC := VNPERDESCFIN_CLIENT;
                END IF; -- 23

                IF vrPARAMETROS.CON_ABATERIMPOSTOSDESCFIN = 'S' THEN

                  IF (NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'N') THEN

                    IF vni = 1 THEN
                      oCONTASRECEBER.VALORDESC := CASE NVL(VSAGREGARVALORSTDESCFIN,
                                                       'N')
                                                    WHEN 'S' THEN
                                                     ROUND((oCONTASRECEBER.VALOR -
                                                           ((/*VNVALORPARCIPI +  */
                                                           VNDIFERIPI) +
                                                           (--VNVALORPARCST +
                                                           VNDIFERST))) *
                                                           (oCONTASRECEBER.PERDESC / 100),
                                                           2)
                                                    WHEN 'N' THEN
                                                     ROUND((oCONTASRECEBER.VALOR -
                                                           (--VNVALORPARCIPI +
                                                           VNDIFERIPI)) *
                                                           (oCONTASRECEBER.PERDESC / 100),
                                                           2)
                                                  END;
                    ELSE
                      IF (VNI <= VNNUMPARCIMPOSTOS) THEN
                        oCONTASRECEBER.VALORDESC := CASE NVL(VSAGREGARVALORSTDESCFIN,
                                                         'N')
                                                      WHEN 'S' THEN
                                                       ROUND((oCONTASRECEBER.VALOR /*-
                                                             ((VNVALORPARCIPI) +
                                                             (VNVALORPARCST))*/) *
                                                             (oCONTASRECEBER.PERDESC / 100),
                                                             2)
                                                      WHEN 'N' THEN
                                                       ROUND((oCONTASRECEBER.VALOR /*-
                                                             (VNVALORPARCIPI)*/) *
                                                             (oCONTASRECEBER.PERDESC / 100),
                                                             2)
                                                    END;
                      ELSE
                        oCONTASRECEBER.VALORDESC := ROUND(oCONTASRECEBER.VALOR *
                                                          (oCONTASRECEBER.PERDESC / 100),
                                                          2);
                      END IF;
                    END IF;

                  ELSE

                    IF VNI = 1 THEN
                      oCONTASRECEBER.VALORDESC := ROUND((oCONTASRECEBER.VALOR -
                                                        (--VNVALORPARCIPI +
                                                        VNDIFERIPI)) *
                                                        (oCONTASRECEBER.PERDESC / 100),
                                                        2);

                    ELSE
                      IF (VNI <= VNNUMPARCIMPOSTOS) THEN
                        oCONTASRECEBER.VALORDESC := ROUND((oCONTASRECEBER.VALOR /*-
                                                          VNVALORPARCIPI*/) *
                                                          (oCONTASRECEBER.PERDESC / 100),
                                                          2);
                      ELSE
                        oCONTASRECEBER.VALORDESC := ROUND(oCONTASRECEBER.VALOR *
                                                          (oCONTASRECEBER.PERDESC / 100),
                                                          2);
                      END IF;
                    END IF;
                  END IF;

                ELSE
                  IF (NVL(VSUTILIZAPROCESSOGERACAOCPST, 'N') = 'N') THEN
                    IF vni = 1 THEN
                      oCONTASRECEBER.VALORDESC := CASE NVL(VSAGREGARVALORSTDESCFIN,
                                                       'N')
                                                    WHEN 'S' THEN
                                                     ROUND((oCONTASRECEBER.VALOR -
                                                           (--VNVALORPARCST +
                                                           VNDIFERST)) *
                                                           (oCONTASRECEBER.PERDESC / 100),
                                                           2)
                                                    WHEN 'N' THEN
                                                     ROUND((oCONTASRECEBER.VALOR) *
                                                           (oCONTASRECEBER.PERDESC / 100),
                                                           2)
                                                  END;
                    ELSE
                      IF (VNI <= VNNUMPARCIMPOSTOS) THEN
                        oCONTASRECEBER.VALORDESC := CASE NVL(VSAGREGARVALORSTDESCFIN,
                                                         'N')
                                                      WHEN 'S' THEN
                                                       ROUND((oCONTASRECEBER.VALOR /*-
                                                             (VNVALORPARCST)*/) *
                                                             (oCONTASRECEBER.PERDESC / 100),
                                                             2)
                                                      WHEN 'N' THEN
                                                       ROUND((oCONTASRECEBER.VALOR) *
                                                             (oCONTASRECEBER.PERDESC / 100),
                                                             2)
                                                    END;
                      ELSE
                        oCONTASRECEBER.VALORDESC := ROUND(oCONTASRECEBER.VALOR *
                                                          (oCONTASRECEBER.PERDESC / 100),
                                                          2);
                      END IF;
                    END IF;
                  ELSE
                    oCONTASRECEBER.VALORDESC := ROUND(oCONTASRECEBER.VALOR *
                                                      (oCONTASRECEBER.PERDESC / 100),
                                                      2);
                  END IF;
                END IF; -- 24
                --------------------- Fim Desconto Financeiro ----------------------

                oCONTASRECEBER.OBS2               := SUBSTR(CAB.OBS1, 1, 100); -- 25
                oCONTASRECEBER.CODFILIALNF        := CAB.CODFILIALNF; -- 26
                oCONTASRECEBER.NUMPED             := CAB.NUMPED; -- 27
                oCONTASRECEBER.CODESTABELECIMENTO := CAB.CODESTABELECIMENTO; -- 28

                oCONTASRECEBER.CODFUNCCHECKOUT := CASE
                                                    WHEN FATURAMENTO.G_FUNC.NUMCAIXABALCAO = 0 THEN
                                                     NULL
                                                    ELSE
                                                     FATURAMENTO.G_FUNC.MATRICULA
                                                  END; -- 29

                oCONTASRECEBER.CODEMITENTEPEDIDO   := CAB.CODEMITENTE; -- 30
                oCONTASRECEBER.BLOQDESDEMITENTEDIF := VSBLOQDESDEMITENTEDIF_PREST; -- 31
                oCONTASRECEBER.DTCRIACAO           := PDDATAEMICAO; -- 32
                oCONTASRECEBER.DTEMISSAOORIG       := PDDATAEMICAO; -- 33

              /*oCONTASRECEBER.PERCOM2 := CASE VSPAGCOMISSAO_COB
                                          WHEN 'N' THEN
                                           0
                                          ELSE
                                           CASE
                                               WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                                0
                                               ELSE
                                                CASE VRPARAMETROS.ABATERDESCFINCOMISSAORCA
                                                  WHEN 'S' THEN
                                                   ROUND(((VNCOMISSAO2 /
                                                         (((VNTOTALGER - G_DESCFIN.VLDESCTOT) -
                                                         (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                         NVL(VNCREDITO, 0))) * 100),
                                                         5) --- 207547
                                                  WHEN 'N' THEN
                                                   ROUND(((VNCOMISSAO2 /
                                                         ((VNTOTALGER -
                                                         (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                       NVL(VNCREDITO, 0))) * 100),
                                                       5)
                                              END
                                           END
                                        END, 100); -- 34

              oCONTASRECEBER.PERCOM3 := LEAST(CASE VSPAGCOMISSAO_COB
                                          WHEN 'N' THEN
                                           0
                                          ELSE
                                           CASE
                                               WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                                0
                                               ELSE
                                                CASE VRPARAMETROS.ABATERDESCFINCOMISSAORCA
                                                  WHEN 'S' THEN
                                                   ROUND(((VNCOMISSAO3 /
                                                         (((VNTOTALGER - G_DESCFIN.VLDESCTOT) -
                                                         (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                         NVL(VNCREDITO, 0))) * 100),
                                                         5) --- 207547
                                                  WHEN 'N' THEN
                                                   ROUND(((VNCOMISSAO3 /
                                                         ((VNTOTALGER -
                                                         (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                       NVL(VNCREDITO, 0))) * 100),
                                                       5)
                                              END
                                           END
                                        END, 100); -- 35

              oCONTASRECEBER.PERCOM4 := LEAST(CASE VSPAGCOMISSAO_COB
                                          WHEN 'N' THEN
                                           0
                                          ELSE
                                           CASE
                                               WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                                0
                                               ELSE
                                                CASE VRPARAMETROS.ABATERDESCFINCOMISSAORCA
                                                  WHEN 'S' THEN
                                                   ROUND(((VNCOMISSAO4 /
                                                         (((VNTOTALGER - G_DESCFIN.VLDESCTOT) -
                                                         (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                         NVL(VNCREDITO, 0))) * 100),
                                                         5) --- 207547
                                                  WHEN 'N' THEN
                                                   ROUND(((VNCOMISSAO4 /
                                                         ((VNTOTALGER -
                                                         (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                       NVL(VNCREDITO, 0))) * 100),
                                                       5)
                                              END
                                           END
                                        END;*/ -- 36

                oCONTASRECEBER.CODUSUR2          := CAB.CODUSUR2; -- 37
                oCONTASRECEBER.CODUSUR3          := CAB.CODUSUR3; -- 38
                oCONTASRECEBER.CODUSUR4          := CAB.CODUSUR4; -- 39
                oCONTASRECEBER.CODAUTORIZACAOTEF := CAB.CODAUTORIZACAOTEF; --40
                oCONTASRECEBER.NSUTEF            := P_FORMAPGTOPEDIDO.NUMAUTORIZACAO;  --41   /* CAB.NSUTEF;*/
                oCONTASRECEBER.CODADMCARTAO      := CAB.CODADMCARTAO; --42

                IF (VNI = 1) AND -- PRIMEIRA PARCELA
                   (vrPARAMETROS.CON_COBRARVLTARIFAPARC1 = 'N') AND --NÃO COBRAR BOLETO NA PRIMEIRA PARCELA
                   (vrPARAMETROS.CON_SOMATXBOLETO = 'S') THEN
                  oCONTASRECEBER.SOMATXBOLETO := 'N'; -- PARA NÃO GERAR VALOR DE BOLETO AO DESDOBRAR
                ELSE
                  oCONTASRECEBER.SOMATXBOLETO := VSSOMATXBOLETO; --vrPARAMETROS.CON_SOMATXBOLETO; -- 43
                END IF;

                oCONTASRECEBER.VALORDESCORIG := oCONTASRECEBER.VALORDESC; -- 44
                oCONTASRECEBER.DTDESC        := VDDTDESC_PREST; -- 45
                oCONTASRECEBER.PRESTTEF      := TO_CHAR(fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA)+--VNPREST +
                                                        VIPRESTTEF_DESC); -- 46
                oCONTASRECEBER.NUMCHECKOUT   := faturamento.G_FUNC.NUMCAIXABALCAO; --47
                oCONTASRECEBER.CODMOTORISTA  := pcodmotorista; --52

                --HIS.05228.2014
                IF NVL(CAB.CODBANCOCM, 0) <> 0 THEN
                  oCONTASRECEBER.CODBANCO := CAB.CODBANCOCM;
                ELSE
                  oCONTASRECEBER.CODBANCO := NULL;
                END IF;
                ----------------

                ----------------
                oCONTASRECEBER.VLTROCO        := P_FORMAPGTOPEDIDO.VLRTROCO;
                OCONTASRECEBER.CODCOBSEFAZ    := VSCODCOBSEFAZ;
                IF (nvl(P_FORMAPGTOPEDIDO.NUMPED, 0) > 0) THEN
                  OCONTASRECEBER.CODCOBSEFAZ    := P_FORMAPGTOPEDIDO.CODCOBSEFAZ;
                END IF;
                oCONTASRECEBER.BANDEIRACARTAO := P_FORMAPGTOPEDIDO.CODBANDEIRACARTAO;
                oCONTASRECEBER.CNPJCREDENCCARTAO := P_FORMAPGTOPEDIDO.CNPJCREDCARTAO;
                oCONTASRECEBER.CORRESPONDENTE := P_FORMAPGTOPEDIDO.TIPOINTEGRACAO;
                ----------------

                --PERSISTINDO INFORMAÇÕES

                BEGIN
                  IF oCONTASRECEBER.VALOR > 0 THEN
                    oCONTASRECEBER.INSERIR;

                    IF (vrPARAMETROS.BAIXARBONFICFATURAMENTO = 'S') AND
                       (NVL(VSCODCOB_PEDC, 'X') IN
                       ('BNF', 'BNFT', 'BNFR', 'BNTR', 'BNRP')) THEN
                      oCONTASRECEBER.VPAGO        := oCONTASRECEBER.VALOR;
                      oCONTASRECEBER.DTPAG        := pddataemicao;
                      oCONTASRECEBER.DTFECHA      := pddataemicao;
                      oCONTASRECEBER.DTBAIXA      := pddataemicao;
                      oCONTASRECEBER.CODBAIXA     := FATURAMENTO.G_FUNC.MATRICULA;
                      oCONTASRECEBER.CODFUNCFECHA := FATURAMENTO.G_FUNC.MATRICULA;
                      oCONTASRECEBER.HORAFECHA    := TO_CHAR(SYSDATE, 'hh24');
                      oCONTASRECEBER.MINUTOFECHA  := TO_CHAR(SYSDATE, 'mi');
                      oCONTASRECEBER.DTCXMOT      := pddataemicao;

                      oCONTASRECEBER.BAIXAR_BONIFIC;
                    END IF;
                  ELSE
                    oCONTASRECEBER.LIMPAR;
                    EXIT;
                  END IF;
                EXCEPTION
                  WHEN OTHERS THEN
                    FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                    FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                    FATURAMENTO.vrPARAMETROS.MSG    := 'Erro ao gerar a parcela comum. prest: ' || vnprest;
                    FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                          (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                           '%ORA-%' THEN
                                           DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                           TO_CHAR($$PLSQL_LINE) END));

                    VBPULOUPEDIDO := TRUE;
                    -- GOTO FIM_LOOP_CAB;

                END;


                ---------------------------FIM PARCELA COMUM---------------------------------
                FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                FATURAMENTO.vrPARAMETROS.MSG    := 'Final da parcela comum. PREST = ' ||
                                                   oCONTASRECEBER.PREST;--vnprest;
                FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, $$PLSQL_LINE);

                vnprest := vnprest + 1;
              END IF;

              EXIT WHEN vni = vnnumparc;
              vni := vni + 1;
            END LOOP;

            -- 5113.014099.2016 - Define percentual de comissão para a parcela de entrada.
            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
            retornarPERCOM(pnNUMTRANSVENDA   => VN_NUMTRANSVENDA,
                           vrPARAMETROS      => vrPARAMETROS,
                           VNNUMPARCCOB      => VNNUMPARCCOB,
                           PSCODCOB          => vscodcob_pedc,
                           PSTIPOPARCELA     => 'COMUM',
                           OUTPERCOM         => vnPERCOM_ENTRADA,
                           OUTPERCOM2        => vnPERCOM_ENTRADA2,
                           OUTPERCOM3        => vnPERCOM_ENTRADA3,
                           OUTPERCOM4        => vnPERCOM_ENTRADA4,
                           OUTPERCOMLIQ      => vnPERCOMLIQ_ENTRADA);
            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido

            --Tarefa: 204140
            IF (VNVLENTRADA > 0) AND VBCRECLI THEN
              --ATENÇÃO!!! NÃO ALTERAR A EXPRESSÃO ABAIXO ("vnnumparc + 1"), POIS EXISTEM
              --DIVERSAS REGRAS REFERENTE A PRIMEIRA PRESTAÇÃO, POR ISSO A PREST DE ENTRADA SERÁ SEMPRE A ÚLTIMA!
              P_GERARPARCELAENTRADA(vnvlentrada,
                                    fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),--(vnnumparc + 2),
                                    pcodmotorista,
                                    vnPERCOM_ENTRADA,
                                    vnPERCOM_ENTRADA2,
                                    vnPERCOM_ENTRADA3,
                                    vnPERCOM_ENTRADA4,
                                    vnPERCOMLIQ_ENTRADA,
                                    ROW_PCNFSAID,
                                    CAB,
                                    vrPARAMETROS); --Tarefa: 211867

              IF (vnTotal_TV1Bnf > 0) THEN
                P_GERARPARCELA_BNF(vnTotal_TV1Bnf,
                                   fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),--(vnnumparc + 3),
                                   pcodmotorista,
                                   ROW_PCNFSAID,
                                   CAB,
                                   vrPARAMETROS);
              END IF;

            ELSIF (VNVLENTRADA > 0) THEN
              P_GERARPARCELAENTRADA(VNVLENTRADA,
                                    fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),--(VNNUMPARC + 1),
                                    pcodmotorista,
                                    vnPERCOM_ENTRADA,
                                    vnPERCOM_ENTRADA2,
                                    vnPERCOM_ENTRADA3,
                                    vnPERCOM_ENTRADA4,
                                    vnPERCOMLIQ_ENTRADA,
                                    ROW_PCNFSAID,
                                    CAB,
                                    vrPARAMETROS);

              IF (vnTotal_TV1Bnf > 0) THEN
                P_GERARPARCELA_BNF(vnTotal_TV1Bnf,
                                   fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),--(vnnumparc + 2),
                                   pcodmotorista,
                                   ROW_PCNFSAID,
                                   CAB,
                                   vrPARAMETROS);
              END IF;

            ELSIF (vnTotal_TV1BNF > 0) AND VBCRECLI THEN
              P_GERARPARCELA_BNF(vnTotal_TV1Bnf,
                                 fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),--(vnnumparc + 2),
                                 pcodmotorista,
                                 ROW_PCNFSAID,
                                 CAB,
                                 vrPARAMETROS);

            ELSIF (vnTotal_TV1Bnf > 0) THEN
              P_GERARPARCELA_BNF(vnTotal_TV1Bnf,
                                 fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),--(vnnumparc + 1),
                                 pcodmotorista,
                                 ROW_PCNFSAID,
                                 CAB,
                                 vrPARAMETROS);
            END IF;

            /* 0.084955.2017 */
            IF NVL(P_FORMAPGTOPEDIDO.VLRTROCO, 0) > 0 THEN
              P_GERARPARCELATROCO(P_FORMAPGTOPEDIDO.VLRTROCO,
                                  fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA),
                                  PCODMOTORISTA,
                                  ROW_PCNFSAID,
                                  cab,
                                  vrPARAMETROS);
            END IF;
            /* 0.084955.2017 */

            IF (((((vntotalger <= 0) OR (vntotalger > 0)) AND
               (vncredito > 0)) OR
               (((cab.codcob = 'BK') OR (vsboleto = 'S')) AND
               (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S') AND
               ((vntotalger - (vnjuros * vnnumparccob)) <= 0) AND
               (vncredito > 0))) AND (NOT vbnaogerarpcprest_cred)) -- Tarefa 56225
             THEN

              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando parcelas do contas a receber(PCPREST)...';

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                    (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                     '%ORA-%' THEN
                                     DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                     TO_CHAR($$PLSQL_LINE) END));

              -- Tarefa 147160 (Tratamento para gerar ST em outra parcela quando existe crédito de cliente)
              IF vntotalger <= 0 THEN
                vnicmsretido := 0;
              END IF;

              IF ((vncredito + vnicmsretido) > vntotalger_ant) THEN
                vnicmsretido := vntotalger_ant - vncredito;
              END IF;

              vipresttef_desc := -1;
              -- Fim tarefa 147160
              VALIDARINSERIRCOBRANCA('CRED', PVC2MENSSAGEN);

              IF PVC2MENSSAGEN = 'OK' THEN
                SELECT NVL(PAGCOMISSAO, 'N')
                  INTO VSPAGCOMISSAO_COB
                  FROM PCCOB
                 WHERE CODCOB = 'CRED';
              ELSE
                FATURAMENTO.VRPARAMETROS.NUMPED := CAB.NUMPED;
                FATURAMENTO.VRPARAMETROS.DATA   := SYSDATE;
                FATURAMENTO.VRPARAMETROS.MSG    := 'Cobrança CRED não encontrada!';

                FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                      (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                       '%ORA-%' THEN
                                       DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                       TO_CHAR($$PLSQL_LINE) END));

                VBPULOUPEDIDO := TRUE;
                --                    GOTO FIM_LOOP_CAB;
              END IF;

              ----------------------PRESTAÇÃO DE CRÉDITO DE CLIENTE----------------------------

              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando crédito. VNCREDITO ' ||
                                                 VNCREDITO;
              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, $$PLSQL_LINE);

              oCONTASRECEBER.LIMPAR; --Limpando informações do objeto

              oCONTASRECEBER.CODFILIAL     := CAB.CODFILIAL; -- 01
              oCONTASRECEBER.CODCLI        := CAB.CODCLI; -- 02
              oCONTASRECEBER.CODUSUR       := CAB.CODUSUR; -- 03
              oCONTASRECEBER.PREST         := fnc_getproximaprest(ROW_PCNFSAID.NUMTRANSVENDA);--VNPREST; -- 04
              oCONTASRECEBER.DUPLIC        := VN_NUMNOTA; -- 05
              oCONTASRECEBER.VALOR         := VNCREDITO; -- 06
              oCONTASRECEBER.CODCOB        := 'CRED'; -- 07
              oCONTASRECEBER.DTVENC        := PDDATAEMICAO; -- 08
              oCONTASRECEBER.DTVENCORIG    := PDDATAEMICAO; -- 09
              oCONTASRECEBER.DTEMISSAO     := PDDATAEMICAO; -- 10
              oCONTASRECEBER.OPERACAO      := 'N'; -- 11
              oCONTASRECEBER.BOLETO        := '1'; -- 12
              oCONTASRECEBER.STATUS        := 'A'; -- 13
              oCONTASRECEBER.NUMCAR        := CAB.NUMCAR; -- 14
              oCONTASRECEBER.CODSUPERVISOR := VNCODSUPERVISOR_USUARI; -- 15
              oCONTASRECEBER.NUMTRANSVENDA := VN_NUMTRANSVENDA; -- 16
              oCONTASRECEBER.VLTXBOLETO    := 0; -- 17
              oCONTASRECEBER.VALORLIQCOM   := VNCREDITO; -- 18

            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
            retornarPERCOM(pnNUMTRANSVENDA   => VN_NUMTRANSVENDA,
                           vrPARAMETROS      => vrPARAMETROS,
                           VNNUMPARCCOB      => VNNUMPARCCOB,
                           PSCODCOB          => oCONTASRECEBER.CODCOB,
                           PSTIPOPARCELA     => 'COMUM',
                           OUTPERCOM         => oCONTASRECEBER.PERCOM,
                           OUTPERCOM2        => oCONTASRECEBER.PERCOM2,
                           OUTPERCOM3        => oCONTASRECEBER.PERCOM3,
                           OUTPERCOM4        => oCONTASRECEBER.PERCOM4,
                           OUTPERCOMLIQ      => oCONTASRECEBER.PERCOMLIQ);
            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
            /*oCONTASRECEBER.PERCOM := CASE VSPAGCOMISSAO_COB
                                       WHEN 'N' THEN
                                        0
                                       ELSE
                                        CASE
                                            WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                             0
                                            ELSE
                                             CASE vrparametros.ABATERDESCFINCOMISSAORCA
                                               WHEN 'S' THEN
                                                ROUND(((VNCOMISSAO /
                                                      (((VNTOTALGER -
                                                      NVL(G_DESCFIN.VLDESCTOT, 0)) -
                                                      (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                      NVL(VNCREDITO, 0))) * 100),
                                                      5) --- 207547
                                               WHEN 'N' THEN
                                                ROUND(((VNCOMISSAO /
                                                      ((VNTOTALGER -
                                                      (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                    NVL(VNCREDITO, 0))) * 100),
                                                    5)
                                           END
                                        END
                                     END;*/ -- 19

              oCONTASRECEBER.CODEPTO            := CAB.CODEPTO; -- 20
              oCONTASRECEBER.VALORORIG          := VNCREDITO; -- 21
              oCONTASRECEBER.CODCOBORIG         := 'CRED'; -- 22
              oCONTASRECEBER.PERDESC            := 0; -- 23
              oCONTASRECEBER.VALORDESC          := 0; -- 24  -- VNCREDITO Retirado a pedido do financeiro  2624.044057.2016
              oCONTASRECEBER.OBS2               := SUBSTR(CAB.OBS1, 1, 100); -- 25
              oCONTASRECEBER.CODFILIALNF        := CAB.CODFILIALNF; -- 26
              oCONTASRECEBER.NUMPED             := CAB.NUMPED; -- 27
              oCONTASRECEBER.CODESTABELECIMENTO := CAB.CODESTABELECIMENTO; -- 28

              oCONTASRECEBER.CODFUNCCHECKOUT := CASE
                                                  WHEN FATURAMENTO.G_FUNC.NUMCAIXABALCAO = 0 THEN
                                                   NULL
                                                  ELSE
                                                   FATURAMENTO.G_FUNC.MATRICULA
                                                END; -- 29

              oCONTASRECEBER.CODEMITENTEPEDIDO   := CAB.CODEMITENTE; -- 30
              oCONTASRECEBER.BLOQDESDEMITENTEDIF := VSBLOQDESDEMITENTEDIF_PREST; -- 31
              oCONTASRECEBER.DTCRIACAO           := PDDATAEMICAO; -- 32
              oCONTASRECEBER.DTEMISSAOORIG       := PDDATAEMICAO; -- 33

            /*oCONTASRECEBER.PERCOM2 := CASE VSPAGCOMISSAO_COB
                                        WHEN 'N' THEN
                                         0
                                        ELSE
                                         CASE
                                             WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                              0
                                             ELSE
                                              ROUND(((VNCOMISSAO2 /
                                                    ((VNTOTALGER -
                                                    (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                  NVL(VNCREDITO, 0))) * 100),
                                                  5)
                                         END
                                      END, 100); -- 34

            oCONTASRECEBER.PERCOM3 := LEAST(CASE VSPAGCOMISSAO_COB
                                        WHEN 'N' THEN
                                         0
                                        ELSE
                                         CASE
                                             WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                              0
                                             ELSE
                                              ROUND(((VNCOMISSAO3 /
                                                    ((VNTOTALGER -
                                                    (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                  NVL(VNCREDITO, 0))) * 100),
                                                  5)
                                         END
                                      END, 100); -- 35

            oCONTASRECEBER.PERCOM4 := LEAST(CASE VSPAGCOMISSAO_COB
                                        WHEN 'N' THEN
                                         0
                                        ELSE
                                         CASE
                                             WHEN (NVL(VNTOTALGER, 0) + VNCREDITO = 0) THEN
                                              0
                                             ELSE
                                              ROUND(((VNCOMISSAO4 /
                                                    ((VNTOTALGER -
                                                    (NVL(VNJUROS, 0) * VNNUMPARCCOB)) +
                                                  NVL(VNCREDITO, 0))) * 100),
                                                  5)
                                         END
                                      END; */-- 36

              oCONTASRECEBER.CODUSUR2          := CAB.CODUSUR2; -- 37
              oCONTASRECEBER.CODUSUR3          := CAB.CODUSUR3; -- 38
              oCONTASRECEBER.CODUSUR4          := CAB.CODUSUR4; -- 39
              oCONTASRECEBER.CODAUTORIZACAOTEF := CAB.CODAUTORIZACAOTEF; --40
              oCONTASRECEBER.NSUTEF            := CAB.NSUTEF; --41
              oCONTASRECEBER.CODADMCARTAO      := CAB.CODADMCARTAO; --42
              oCONTASRECEBER.SOMATXBOLETO      := 'N'; -- 43
              oCONTASRECEBER.PRESTTEF          := oCONTASRECEBER.PREST;--VNPREST; -- 44
              oCONTASRECEBER.NUMCHECKOUT       := FATURAMENTO.G_FUNC.NUMCAIXABALCAO; --45
              oCONTASRECEBER.CODMOTORISTA      := pcodmotorista; --52

              BEGIN
                SELECT CODCOBSEFAZ
                  INTO OCONTASRECEBER.CODCOBSEFAZ
                  FROM pccob
                WHERE pccob.codcob = oCONTASRECEBER.CODCOB;
              EXCEPTION
                WHEN OTHERS THEN
                  OCONTASRECEBER.CODCOBSEFAZ := '99';
              END;

              --PERSISTINDO INFORMAÇÕES
              oCONTASRECEBER.INSERIR;

              --A PRESTAÇÃO DE CRÉDITO É GERADA ACERTADA E QUITADA
              oCONTASRECEBER.ACERTAR;
              oCONTASRECEBER.QUITAR;

              vnprest := vnprest + 1;

              IF ((vntotalger <= 0) AND (vnvlentrada = 0)) THEN
                --EXIT;
              --ELSE
                vbnaogerarpcprest_cred := TRUE;
                -- Tarefa 54105
              END IF;
            END IF;


            --207547
            IF (ROW_PCNFSAID.VLTOTGER > 0) then
              UPDATE PCNFSAID
                 SET VLDESCFIN     = G_DESCFIN.VLDESCTOT,
                     PERDESCFINTOT =
                     ((ROW_PCNFSAID.VLTOTGER -
                     (ROW_PCNFSAID.VLTOTGER - G_DESCFIN.VLDESCTOT)) /
                     ROW_PCNFSAID.VLTOTGER * 100)
               WHERE PCNFSAID.NUMTRANSVENDA = VN_NUMTRANSVENDA;
            END IF;

            -- fim tarefa 207547

            --Tarefa 120404
            vdvalorparcelarestricao := 0;

            SELECT COUNT(1)
              INTO vicontador
              FROM pcnfsaid
             WHERE pcnfsaid.numtransvenda = VN_NUMTRANSVENDA
               AND pcnfsaid.numnota = VN_NUMNOTA;

            IF vicontador > 0 THEN
              SELECT NVL(pcnfsaid.vltotal, pcnfsaid.vltotger)
                INTO vdvalorparcelarestricao
                FROM pcnfsaid
               WHERE pcnfsaid.numtransvenda = VN_NUMTRANSVENDA
                 AND pcnfsaid.numnota = VN_NUMNOTA;
            END IF;

            /*
            --Valor total da nota
            IF ((TO_NUMBER(REPLACE(NVL(vsvlminvendabk, '0'),
                                   ',',
                                   '.')) >
               vdvalorparcelarestricao) AND
               (vrPARAMETROS.CON_TIPOVLMINVENDABK = 'VT')) AND
               (vsboleto = 'S') AND (cab.numpedorigem IS NULL) AND
               (cab.restricaotransp = 'N') THEN
              vscodcob_pedc := 'CH';

              UPDATE pcnfsaid
                 SET codcob = 'CH'
               WHERE pcnfsaid.numtransvenda =
                     VN_NUMTRANSVENDA
                 AND pcnfsaid.numnota = VN_NUMNOTA;

              UPDATE pcprest
                 SET codcob = 'CH', codcoborig = 'CH'
               WHERE pcprest.numtransvenda =
                     VN_NUMTRANSVENDA
                 AND pcprest.duplic = VN_NUMNOTA
                 AND pcprest.codcob NOT IN
                     ('CRED', 'DESD', 'ESTR');
            END IF;*/

            vdvalorparcelarestricao := 0;

            SELECT COUNT(1)
              INTO vicontador
              FROM pcprest
             WHERE pcprest.numtransvenda = VN_NUMTRANSVENDA
               AND pcprest.codcob NOT IN ('CRED', 'DESD', 'ESTR')
               AND pcprest.duplic = VN_NUMNOTA
               AND ROWNUM = 1;

            IF vicontador > 0 THEN
              SELECT pcprest.valor
                INTO vdvalorparcelarestricao
                FROM pcprest
               WHERE pcprest.numtransvenda = VN_NUMTRANSVENDA
                 AND pcprest.codcob NOT IN ('CRED', 'DESD', 'ESTR')
                 AND pcprest.duplic = VN_NUMNOTA
                 AND ROWNUM = 1;
            END IF;

            /*
            --Valor da prestação
            IF ((TO_NUMBER(REPLACE(NVL(vsvlminvendabk, '0'),
                                   ',',
                                   '.')) >
               vdvalorparcelarestricao) AND
               (vrPARAMETROS.CON_TIPOVLMINVENDABK = 'VP')) AND
               (vsboleto = 'S') AND (cab.numpedorigem IS NULL) AND
               (cab.restricaotransp = 'N') THEN
              vscodcob_pedc := 'CH';

              UPDATE pcnfsaid
                 SET codcob = 'CH'
               WHERE pcnfsaid.numtransvenda =
                     VN_NUMTRANSVENDA
                 AND pcnfsaid.numnota = VN_NUMNOTA;

              UPDATE pcprest
                 SET codcob = 'CH', codcoborig = 'CH'
               WHERE pcprest.numtransvenda =
                     VN_NUMTRANSVENDA
                 AND pcprest.duplic = VN_NUMNOTA
                 AND pcprest.codcob NOT IN
                     ('CRED', 'DESD', 'ESTR');
            END IF; -- Valor da prestação
            */
          END IF;

   else
    VBPULOUPEDIDO := gerarFinanceiroAutosservico(ROW_PCNFSAID, P_CAB, vrPARAMETROS);
   end if;

         --  vsformaparcelamento_plpag = 'T'

        --Tarefa 124971
        ------------------------------------------------------ GERAR PARCELA ST E STGNRE ---------------------------------------------------------
        DECLARE
          VBGEROUCREDTOTAL BOOLEAN := FALSE;
          --VBGEROUCREDparcial BOOLEAN := FALSE;
          -- 1636.030531.2016
          PROCEDURE GERACREDPRESTST IS
            ROWPCCRECLI PCCRECLI%ROWTYPE;
            VNCREDITOST NUMBER;
          BEGIN
            VNCREDITOST := 0;
            FOR CRED IN (SELECT CODCLI,
                                DTLANC,
                                CODFILIAL,
                                ORIGEM,
                                NUMNOTA,
                                NVL(NUMTRANSVENDA, 0) NUMTRANSVENDA,
                                NUMTRANSENTDEVCLI,
                                VALOR,
                                NVL(NUMTRANS, 0) NUMTRANS,
                                NVL(NUMLANC, 0) NUMLANC,
                                SERIE,
                                HISTORICO,
                                NUMPED,
                                DTEMISSAOTITULO,
                                ROWID
                           FROM PCCRECLI
                          WHERE CODCLI = CAB.CODCLI
                            AND DTDESCONTO IS NULL
                            AND (VRPARAMETROS.USACREDCLITODASFILIAIS = 'S' OR
                                CODFILIAL = CAB.CODFILIAL)
                            AND NVL(NUMPED, 0) = 0
                            AND SITUACAO IS NULL
                          ORDER BY VALOR) LOOP
              BEGIN
                IF CRED.VALOR > VNVALORPARCST THEN
                  VNCREDITOST := VNCREDITOST + VNVALORPARCST;

                  -- TAREFA 54105
                  UPDATE PCCRECLI
                     SET DTDESCONTO        = TRUNC(SYSDATE),
                         CODFILIALDESC     = CAB.CODFILIAL,
                         NUMNOTADESC       = VN_NUMNOTA,
                         SERIEDESC         = VC2SERIE_DOCC,
                         CODFUNC           = FATURAMENTO.G_FUNC.MATRICULA,
                         HORA              = TO_CHAR(SYSDATE, 'HH24'),
                         MINUTO            = TO_CHAR(SYSDATE, 'MI'),
                         NUMTRANSVENDADESC = VIPROXNUMTRANSVENDACPST,
                         VALOR             = VNVALORPARCST --VNTOTALGERTEMP
                   WHERE CODCLI = CAB.CODCLI
                        /*TAREFA: 192716                                                                                                                                                                                                                                                                                                                                                                                                                  NVL(CODFILIAL, '99')) IN (CAB.CODFILIAL, '99')*/
                     AND (VRPARAMETROS.USACREDCLITODASFILIAIS = 'S' OR
                         CODFILIAL = CAB.CODFILIAL)
                        --FIM TAREFA: 192716
                     AND NVL(NUMNOTA, 0) = NVL(CRED.NUMNOTA, 0)
                     AND NVL(NUMTRANSENTDEVCLI, 0) =
                         NVL(CRED.NUMTRANSENTDEVCLI, 0)
                     AND NVL(NUMTRANSVENDA, 0) = NVL(CRED.NUMTRANSVENDA, 0)
                     AND NVL(NUMTRANS, 0) = NVL(CRED.NUMTRANS, 0)
                     AND NVL(NUMLANC, 0) = NVL(CRED.NUMLANC, 0)
                     AND DTLANC = CRED.DTLANC
                     AND DTDESCONTO IS NULL
                     AND SITUACAO IS NULL
                        -- TAREFA 59125
                     AND ROWID = CRED.ROWID
                     AND ROWNUM = 1;

                  ROWPCCRECLI.CODCLI            := CRED.CODCLI;
                  ROWPCCRECLI.DTLANC            := CRED.DTLANC;
                  ROWPCCRECLI.CODFILIAL         := CRED.CODFILIAL;
                  ROWPCCRECLI.ORIGEM            := CRED.ORIGEM;
                  ROWPCCRECLI.NUMNOTA           := CRED.NUMNOTA;
                  ROWPCCRECLI.SERIE             := CRED.SERIE;
                  ROWPCCRECLI.VALOR             := (CRED.VALOR -
                                                   VNVALORPARCST);
                  ROWPCCRECLI.NUMTRANSVENDA     := CRED.NUMTRANSVENDA;
                  ROWPCCRECLI.NUMTRANSENTDEVCLI := CRED.NUMTRANSENTDEVCLI;
                  ROWPCCRECLI.NUMTRANS          := CRED.NUMTRANS;
                  ROWPCCRECLI.NUMLANC           := CRED.NUMLANC;
                  ROWPCCRECLI.HISTORICO         := CRED.HISTORICO;
                  ROWPCCRECLI.NUMPED            := CRED.NUMPED;
                  ROWPCCRECLI.CODFUNCLANC       := FATURAMENTO.G_FUNC.MATRICULA;
                  ROWPCCRECLI.DTEMISSAOTITULO   := CRED.DTEMISSAOTITULO;
                  INSERT INTO PCCRECLI VALUES ROWPCCRECLI;

                  VBGEROUCREDTOTAL := TRUE;

                  EXIT; -- SAIR DO LOOP DO PCCRECLI

                  IF VNVALORPARCST <= 0 THEN
                    EXIT;
                  END IF;
                  /*ELSE

                  vncreditoST   := vncreditoST + cred.valor;
                  VNVALORPARCST := VNVALORPARCST - cred.valor;

                  UPDATE pccrecli
                     SET dtdesconto        = TRUNC(SYSDATE),
                         codfilialdesc     = cab.codfilial,
                         numnotadesc       = VN_NUMNOTA,
                         seriedesc         = vc2serie_docc,
                         codfunc           = FATURAMENTO.G_FUNC.MATRICULA,
                         hora              = TO_CHAR(SYSDATE, 'HH24'),
                         minuto            = TO_CHAR(SYSDATE, 'MI'),
                         numtransvendadesc = VIPROXNUMTRANSVENDACPST
                   WHERE codcli = cab.codcli
                        \*Tarefa: 192716
                                                                                                                                                                                                              NVL(codfilial, '99')) IN (cab.codfilial, '99')*\
                     AND (FATURAMENTO.vrPARAMETROS.USACREDCLITODASFILIAIS = 'S' OR
                         codfilial = cab.codfilial)
                        --Fim Tarefa: 192716
                     AND NVL(numnota, 0) = NVL(cred.numnota, 0)
                     AND NVL(numtransentdevcli, 0) = NVL(cred.numtransentdevcli, 0)
                     AND NVL(numtransvenda, 0) = NVL(cred.numtransvenda, 0)
                     AND NVL(numtrans, 0) = NVL(cred.numtrans, 0)
                     AND NVL(numlanc, 0) = NVL(cred.numlanc, 0)
                     AND dtlanc = cred.dtlanc
                     AND dtdesconto IS NULL
                     AND situacao IS NULL
                        --Tarefa 59125
                     AND ROWID = cred.ROWID
                     AND ROWNUM = 1;
                  VBGEROUCREDparcial := true;*/
                END IF;
              EXCEPTION
                WHEN OTHERS THEN
                  PVC2MENSSAGEN := SQLCODE || '-' || SQLERRM || CHR(13) ||
                                   ' -21- ATUALIZACAO DO CREDITO DE CLIENTE.' ||
                                   CHR(13) || ' - ' ||
                                   DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
                  RAISE;
              END;
            END LOOP;
          END; -- FIM GERARCREDPRESTST --------------

          PROCEDURE CARREGCONTASRECEBERPRESTST IS
          BEGIN

            oCONTASRECEBER.CODFILIAL           := CAB.CODFILIAL; -- 01
            oCONTASRECEBER.CODCLI              := CAB.CODCLI; -- 02
            oCONTASRECEBER.CODUSUR             := CAB.CODUSUR; -- 03
            oCONTASRECEBER.CODUSUR2            := CAB.CODUSUR2; -- 03
            oCONTASRECEBER.CODUSUR3            := CAB.CODUSUR3; -- 03
            oCONTASRECEBER.CODUSUR4            := CAB.CODUSUR4; -- 03
            oCONTASRECEBER.PREST               := 1; -- 04
            oCONTASRECEBER.DUPLIC              := VN_NUMNOTA; -- 05
            oCONTASRECEBER.VALOR               := VNVALORPARCST + VNICMSBNFRETIDO; -- 06
            oCONTASRECEBER.CODCOB              := VRPARAMETROS.CODCOBST; -- 07
            oCONTASRECEBER.DTVENC              := VDDTVENCCPST; -- 08
            oCONTASRECEBER.DTVENCORIG          := VDDTVENCCPST; -- 09
            oCONTASRECEBER.DTEMISSAO           := TRUNC(SYSDATE); -- 10
            oCONTASRECEBER.NUMCAR              := CAB.NUMCAR; -- 11
            oCONTASRECEBER.CODSUPERVISOR       := CAB.CODSUPERVISOR; -- 12
            oCONTASRECEBER.NUMTRANSVENDA       := VIPROXNUMTRANSVENDACPST; -- 13
            oCONTASRECEBER.VALORORIG           := oCONTASRECEBER.VALOR; -- 14
            oCONTASRECEBER.CODCOBORIG          := oCONTASRECEBER.CODCOB; -- 15
            oCONTASRECEBER.CODFILIALNF         := CAB.CODFILIALNF; -- 16
            oCONTASRECEBER.NUMPED              := CAB.NUMPED; -- 17
            oCONTASRECEBER.DTCRIACAO           := PDDATAEMICAO; -- 18
            oCONTASRECEBER.DTEMISSAOORIG       := PDDATAEMICAO; -- 19
            oCONTASRECEBER.STATUS              := 'A'; -- 20
            oCONTASRECEBER.NUMTRANSVENDAST     := VN_NUMTRANSVENDA; -- 21
            oCONTASRECEBER.OPERACAO            := 'N'; -- 22
            oCONTASRECEBER.BOLETO              := '1'; -- 23

            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
            retornarPERCOM(pnNUMTRANSVENDA   => VN_NUMTRANSVENDA,
                           vrPARAMETROS      => vrPARAMETROS,
                           VNNUMPARCCOB      => VNNUMPARCCOB,
                           PSCODCOB          => oCONTASRECEBER.CODCOB,
                           PSTIPOPARCELA     => 'ST',
                           OUTPERCOM         => oCONTASRECEBER.PERCOM,
                           OUTPERCOM2        => oCONTASRECEBER.PERCOM2,
                           OUTPERCOM3        => oCONTASRECEBER.PERCOM3,
                           OUTPERCOM4        => oCONTASRECEBER.PERCOM4,
                           OUTPERCOMLIQ      => oCONTASRECEBER.PERCOMLIQ);

            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido

            /*oCONTASRECEBER.PERCOM              := 0; -- 24
            oCONTASRECEBER.PERCOM2             := 0; -- 25
            oCONTASRECEBER.PERCOM3             := 0; -- 26
            oCONTASRECEBER.PERCOM4             := 0; -- 27
            */


            oCONTASRECEBER.VALORLIQCOM         := oCONTASRECEBER.VALOR; -- 28
            oCONTASRECEBER.VLTXBOLETO          := 0; -- 29
            oCONTASRECEBER.NUMCHECKOUT         := FATURAMENTO.G_FUNC.NUMCAIXABALCAO; -- 30
            oCONTASRECEBER.CODEMITENTEPEDIDO   := CAB.CODEMITENTE; -- 31
            oCONTASRECEBER.BLOQDESDEMITENTEDIF := VSBLOQDESDEMITENTEDIF_PREST; -- 32
            oCONTASRECEBER.SOMATXBOLETO        := VSSOMATXBOLETO; --vrPARAMETROS.CON_SOMATXBOLETO; -- 33
            oCONTASRECEBER.PRESTTEF            := 1;
            oCONTASRECEBER.CODFUNCCHECKOUT := CASE
                                                WHEN FATURAMENTO.G_FUNC.NUMCAIXABALCAO = 0 THEN
                                                 NULL
                                                ELSE
                                                 FATURAMENTO.G_FUNC.MATRICULA
                                              END;
            oCONTASRECEBER.CODMOTORISTA        := pcodmotorista; --52
            oCONTASRECEBER.OBS2                := SUBSTR(CAB.OBS1, 1, 100); --53

          END; -- FIM CARREGCONTASRECEBERPRESTST --------------

          PROCEDURE CARREGCONTASRECEBERPRESTSTGNRE IS
          BEGIN
            oCONTASRECEBER.CODFILIAL           := CAB.CODFILIAL; -- 01
            oCONTASRECEBER.CODCLI              := CAB.CODCLI; -- 02
            oCONTASRECEBER.CODUSUR             := CAB.CODUSUR; -- 03
            oCONTASRECEBER.PREST               := 1; -- 04
            oCONTASRECEBER.DUPLIC              := VN_NUMNOTA; -- 05
            oCONTASRECEBER.VALOR               := VNICMSRETIDOGNRE; -- 06
            oCONTASRECEBER.CODCOB              := VRPARAMETROS.CODCOBST; -- 07
            oCONTASRECEBER.DTVENC              := VDDTVENCCPST; -- 08
            oCONTASRECEBER.DTVENCORIG          := VDDTVENCCPST; -- 09
            oCONTASRECEBER.DTEMISSAO           := TRUNC(SYSDATE); -- 10
            oCONTASRECEBER.NUMCAR              := CAB.NUMCAR; -- 11
            oCONTASRECEBER.CODSUPERVISOR       := CAB.CODSUPERVISOR; -- 12
            oCONTASRECEBER.NUMTRANSVENDA       := VIPROXNUMTRANSVENDACPST; -- 13
            oCONTASRECEBER.VALORORIG           := oCONTASRECEBER.VALOR; -- 14
            oCONTASRECEBER.CODCOBORIG          := oCONTASRECEBER.CODCOB; -- 15
            oCONTASRECEBER.CODFILIALNF         := CAB.CODFILIALNF; -- 16
            oCONTASRECEBER.NUMPED              := CAB.NUMPED; -- 17
            oCONTASRECEBER.DTCRIACAO           := PDDATAEMICAO; -- 18
            oCONTASRECEBER.DTEMISSAOORIG       := PDDATAEMICAO; -- 19
            oCONTASRECEBER.STATUS              := 'A'; -- 20
            oCONTASRECEBER.NUMTRANSVENDAST     := VN_NUMTRANSVENDA; -- 21
            oCONTASRECEBER.OPERACAO            := 'N'; -- 22
            oCONTASRECEBER.BOLETO              := '1'; -- 23
            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido
            retornarPERCOM(pnNUMTRANSVENDA   => VN_NUMTRANSVENDA,
                           vrPARAMETROS      => vrPARAMETROS,
                           VNNUMPARCCOB      => VNNUMPARCCOB,
                           PSCODCOB          => oCONTASRECEBER.CODCOB,
                           PSTIPOPARCELA     => 'ST',
                           OUTPERCOM         => oCONTASRECEBER.PERCOM,
                           OUTPERCOM2        => oCONTASRECEBER.PERCOM2,
                           OUTPERCOM3        => oCONTASRECEBER.PERCOM3,
                           OUTPERCOM4        => oCONTASRECEBER.PERCOM4,
                           OUTPERCOMLIQ      => oCONTASRECEBER.PERCOMLIQ);

            -- 1615.009403.2018 - Ajuste para calcular o percentual de comissão com base do valor líquido

            /*oCONTASRECEBER.PERCOM              := 0; -- 24
            oCONTASRECEBER.PERCOM2             := 0; -- 25
            oCONTASRECEBER.PERCOM3             := 0; -- 26
            oCONTASRECEBER.PERCOM4             := 0; -- 27
            */

            oCONTASRECEBER.VALORLIQCOM         := oCONTASRECEBER.VALOR; -- 28
            oCONTASRECEBER.VLTXBOLETO          := 0; -- 29
            oCONTASRECEBER.NUMCHECKOUT         := FATURAMENTO.G_FUNC.NUMCAIXABALCAO; -- 30
            oCONTASRECEBER.CODEMITENTEPEDIDO   := CAB.CODEMITENTE; -- 31
            oCONTASRECEBER.BLOQDESDEMITENTEDIF := VSBLOQDESDEMITENTEDIF_PREST; -- 32
            oCONTASRECEBER.SOMATXBOLETO        := VSSOMATXBOLETO; --vrPARAMETROS.CON_SOMATXBOLETO; -- 33
            oCONTASRECEBER.PRESTTEF            := 1;
            oCONTASRECEBER.CODFUNCCHECKOUT := CASE
                                                WHEN FATURAMENTO.G_FUNC.NUMCAIXABALCAO = 0 THEN
                                                 NULL
                                                ELSE
                                                 FATURAMENTO.G_FUNC.MATRICULA
                                              END;
            oCONTASRECEBER.CODMOTORISTA        := pcodmotorista; --52
          END; -- FIM CARREGCONTASRECEBERPRESTSTGNRE --------------
        BEGIN

          -----------------------------PARCELA REFERENTE AO VALOR DE ST---------------------------
          IF (NVL(vsutilizaprocessogeracaocpst, 'N') = 'S') AND
             ((vnicmsretido > 0) OR (vnicmsbnfretido > 0)) -- Tarefa 147160
           THEN
            --Buscar o próximo nº de transação.
            viproxnumtransvendacpst := FERRAMENTAS.F_PROX_NUMTRANSVENDA;

            SELECT MIN(pcprest.dtvenc)
              INTO vddtvenccpst
              FROM pcprest
             WHERE pcprest.duplic = VN_NUMNOTA
               AND pcprest.numtransvenda = VN_NUMTRANSVENDA
               AND PCPREST.CODCOB NOT IN ('CRED'); -- Solicitação: 1636.125776.2014

            -- Se a primeira parcela obtiver 100% dos créditos, então neste caso, a variável "vddtvenccpst" fica nula
            -- pois, a consulta acima não retorna valor, isto gera erro de persistência na tabela PCPREST - DTVENC NOT NULL
            IF (vddtvenccpst IS NULL) THEN
              vddtvenccpst := trunc(SYSDATE);
            END IF;

            IF (TRUNC(SYSDATE) + viprazovencst_client) <= vddtvenccpst THEN

              vddtvenccpst := TRUNC(SYSDATE) + viprazovencst_client;

            END IF;

            --Gerar parcela de ST
            FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
            FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
            FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando parcela referente ao valor de ST...';

            FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                  (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                   '%ORA-%' THEN
                                   DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                   TO_CHAR($$PLSQL_LINE) END));

            oCONTASRECEBER.LIMPAR;

            IF /*(VRPARAMETROS.CODCOBST = 'CRED') AND*/
             (vbcrecli) THEN
              -------------------
              GERACREDPRESTST;
              -------------------
            END IF;

            ----------------------------
            CARREGCONTASRECEBERPRESTST;
            ----------------------------
            IF (VBGEROUCREDTOTAL) /*OR (VBGEROUCREDparcial)*/
             THEN
              oCONTASRECEBER.CODCOB := 'CRED';
            END IF;

            BEGIN
              SELECT CODCOBSEFAZ
                INTO OCONTASRECEBER.CODCOBSEFAZ
                FROM pccob
               WHERE pccob.codcob = oCONTASRECEBER.CODCOB;
            EXCEPTION
                WHEN OTHERS THEN
                  OCONTASRECEBER.CODCOBSEFAZ := '99';
            END;

            --PERSISTINDO INFORMAÇÕES
            oCONTASRECEBER.INSERIR;

            IF (VBGEROUCREDTOTAL) /*OR (VBGEROUCREDparcial)*/
             THEN
              oCONTASRECEBER.QUITAR;
            END IF;

            /*                        IF VBGEROUCREDparcial THEN
              oCONTASRECEBER.LIMPAR;
              ----------------------------
              CARREGCONTASRECEBERPRESTST;
              ----------------------------
              oCONTASRECEBER.VALOR := VNVALORPARCST;
              --PERSISTINDO INFORMAÇÕES
              oCONTASRECEBER.INSERIR;
            END IF;*/
          END IF; -- FIM DA PARCELA COM ST

          -----------------------------PARCELA REFERENTE AO VALOR DE ST(GNRE)-------------------------
          IF (NVL(vsutilizaprocessogeracaocpst, 'N') = 'S') AND
             (vnicmsretidognre > 0) THEN
            --Buscar o próximo nº de transação.
            viproxnumtransvendacpst := FERRAMENTAS.F_PROX_NUMTRANSVENDA;

            --Gerar parcela de ST GNRE
            FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
            FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
            FATURAMENTO.vrPARAMETROS.MSG    := 'Gerando parcela referente ao valor de ST(GNRE)...';

            FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                  (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                   '%ORA-%' THEN
                                   DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                   TO_CHAR($$PLSQL_LINE) END));

            oCONTASRECEBER.LIMPAR;
            IF /*(VRPARAMETROS.CODCOBST = 'CRED') AND*/
             (vbcrecli) THEN
              -------------------
              GERACREDPRESTST;
              -------------------
            END IF;
            ----------------------------
            CARREGCONTASRECEBERPRESTSTGNRE;
            ----------------------------
            IF (VBGEROUCREDTOTAL) /*OR (VBGEROUCREDparcial)*/
             THEN
              oCONTASRECEBER.CODCOB := 'CRED';
            END IF;

            begin
             SELECT CODCOBSEFAZ
              INTO OCONTASRECEBER.CODCOBSEFAZ
              FROM pccob
             WHERE pccob.codcob = oCONTASRECEBER.CODCOB;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                OCONTASRECEBER.CODCOBSEFAZ := null;
            end;

            --PERSISTINDO INFORMAÇÕES
            oCONTASRECEBER.INSERIR;

            IF (VBGEROUCREDTOTAL) /*OR (VBGEROUCREDparcial)*/
             THEN
              oCONTASRECEBER.QUITAR;
            END IF;
          END IF;
        END;
        ---------------------------------------------------------------------------------------------------------------

        --Fim tarefa 124971
        IF (vnvlentrada > 0) THEN
          DELETE pcprest
           WHERE NVL(pcprest.valor, 0) = 0
             AND pcprest.numtransvenda = VN_NUMTRANSVENDA;

          UPDATE pcpedc
             SET vlentrada = vnvlentrada
           WHERE pcpedc.numped = cab.numped;
        END IF;

        -- Tarefa 115721 (Transf. cobrança do cabeçalho para CH, a mesma foi alterada na pcprest)
        IF vnnumparc = 1 THEN
          SELECT COUNT(1)
            INTO vicontador
            FROM pcprest
           WHERE numtransvenda = VN_NUMTRANSVENDA
             AND codcob = 'CH';

          IF vicontador = 1 THEN
            UPDATE pcnfsaid
               SET codcob = 'CH'
             WHERE numtransvenda = VN_NUMTRANSVENDA;
          END IF;
        END IF; -- Tarefa 115721
        --END IF;

        IF (G_VNTXBOLETO > 0) AND (vscodcob_pedc IN ('CH', 'DH')) AND
           (VNQT_CONTASRECEBER = 0) AND (CAB.CODCOB NOT IN('CH', 'DH')) THEN
          --Tarefa: 202661 / 210362 (Rafael Braga)
          --ATENÇÃO!!!
          --NÃO ALTERAR ABAIXO...RODRIGO SANTOS
          UPDATE pcnfsaid
             SET vltotal            = vltotal - DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                       'S',
                                                       G_VNTXBOLETO,
                                                       0),
                 vltotger           = vntotalger,
                 vltabela           = vltabela -
                                      decode(nvl(vltabela, 0),
                                             0,
                                             0,
                                             DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                    'S',
                                                    G_VNTXBOLETO,
                                                    0)),
                 vloutrasdesp       = vloutrasdesp -
                                      DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                             'S',
                                             G_VNTXBOLETO,
                                             0),
                 vltotalsemdescicms = vltotalsemdescicms -
                                      decode(nvl(vltotalsemdescicms, 0),
                                             0,
                                             0,
                                             DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                    'S',
                                                    G_VNTXBOLETO,
                                                    0))
           WHERE numtransvenda = VN_NUMTRANSVENDA;

          -- Autor: Deyvid Costa
          -- Descrição: Bloco abaixo retira o valor de boleto da nota e PREST e recalcula outras despesas, frete e ST
          -- CON_SOMATARIFABANCDUPLIC: Adiciona a tarifa do boleto bancário na nota fiscal caso igual a "SIM" - 663.069452.2015
          IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S' THEN
            -- Obtém o valor de outras despesas atual
            BEGIN
              SELECT NVL(VLOUTRASDESP, 0)
                INTO vnvloutrasdesp
                FROM PCPEDC
               WHERE NUMPED = VNNUMPED;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                vnvloutrasdesp := 0;
            END;

            -- !!! MUITA cautela aki!!!!
            -- Realiza o ROLLBACK até o ponto especificado "CONTASRECEBER", que é o início do processamento do contas a receber
            ROLLBACK TO SAVEPOINT CONTASRECEBER;

            -- Tratamento para, caso a rotina de pedido tenha gravado o valor do boleto - 663.069452.2015
            /*          IF (vnvloutrasdesp - G_VNTXBOLETO) = G_VNTXBOLETO THEN
              UPDATE PCPEDC -- vezes 2 para retirar o valor excedente adicionado no começo do processamento
                 SET VLOUTRASDESP = VLOUTRASDESP - (G_VNTXBOLETO * 2)
               WHERE NUMPED = oNFSAIDA.NUMPED;
            ELSE*/
            UPDATE PCPEDC
               SET VLOUTRASDESP = CASE
                                    WHEN (VLOUTRASDESP - G_VNTXBOLETO -
                                         NVL(vnVLSEGURO, 0)) < 0 THEN
                                     0
                                    ELSE
                                     (VLOUTRASDESP - G_VNTXBOLETO -
                                     NVL(vnVLSEGURO, 0))
                                  END,
                   VLATEND      = 0
             WHERE NUMPED = VNNUMPED;
            --                        END IF;

            -- Varre a lista com os valores originais dos itens e os atualuzada na PCPEDI para refazer o cálculo de st - 663.069452.2015
            IF FATURAMENTO.LISTA_PEDIDO_ATUAL.COUNT > 0 THEN
              FOR X IN 1 .. FATURAMENTO.LISTA_PEDIDO_ATUAL.LAST LOOP
                -- Recebe os valores originais dos itens para o recálculo do ST
                UPDATE PCPEDI
                   SET PVENDA       = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).PVENDA,
                       PTABELA      = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).PTABELA,
                       PBASERCA     = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).PBASERCA,
                       ST           = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).ST,
                       BASEICST     = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).BASEICST,
                       VLOUTRASDESP = 0
                 WHERE NUMPED = VNNUMPED
                   AND CODPROD = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).CODPROD
                   AND NUMSEQ = FATURAMENTO.LISTA_PEDIDO_ATUAL(X).NUMSEQ;

                -- Totaliza o VLATEND baseado no valor original para recalcular o Seguro corretamente HIS.02593.2015
                UPDATE PCPEDC
                   SET VLATEND = VLATEND + NVL(FATURAMENTO.LISTA_PEDIDO_ATUAL(X)
                                .PVENDA * FATURAMENTO.LISTA_PEDIDO_ATUAL(X).QT, 0)
                 WHERE NUMPED = VNNUMPED;

              END LOOP;

              UPDATE PCPEDC
                 SET VLATEND = VLATEND + NVL(VLOUTRASDESP, 0) +
                               NVL(VLFRETE, 0)
               WHERE NUMPED = VNNUMPED;

            END IF;

            -- Chama o cálculo de st com o novo valor de outras despesas - 663.069452.2015
            vnvloutrasdesp := CASE
                                WHEN (vnvloutrasdesp - G_VNTXBOLETO -
                                     NVL(vnVLSEGURO, 0)) < 0 THEN
                                 0
                                ELSE
                                 (vnvloutrasdesp - G_VNTXBOLETO - NVL(vnVLSEGURO, 0))
                              END; -- Necessário para o recálculo

            G_VNTXBOLETO             := 0; --Necessário para o recálculo
            FATURAMENTO.G_VNTXBOLETO := 0; --Necessário para o recálculo

            FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
            FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
            FATURAMENTO.vrPARAMETROS.MSG    := 'Realizando rateio de despesas pela package de dados financeiros...';

            FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                    (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                     '%ORA-%' THEN
                                     DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                     TO_CHAR($$PLSQL_LINE) END));

            if vsRateio_NoFinanceiro = 'S' THEN
              -- Calculando o ST já sem o valor do BOLETO
              FATURAMENTO.RATEAR_FRETE_OUTRASDESPESAS(CAB.NUMPED,
                                                      vrPARAMETROS.CON_ALIQICMOUTRASDESP,
                                                      vrPARAMETROS.CON_PERCICMFRETEENT,
                                                      PVC2MENSSAGEN);


              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Realizando rateio de despesas pela package de dados financeiros...' || PVC2MENSSAGEN;

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                    (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                     '%ORA-%' THEN
                                     DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                     TO_CHAR($$PLSQL_LINE) END));

            END IF;

            IF PVC2MENSSAGEN = 'OK' THEN

              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Atualizando dados dos pedidos depois do rateio.';

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                    (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                     '%ORA-%' THEN
                                     DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                     TO_CHAR($$PLSQL_LINE) END));

              -- Atualiza os itens dos pedidos, depois replica para os itens da nota
              FOR ITEM IN (SELECT C.NUMPED,
                                  I.CODPROD,
                                  I.NUMSEQ,
                                  I.PVENDA,
                                  I.QT,
                                  I.ST,
                                  I.VLIPI,
                                  I.BASEICST,
                                  I.VLOUTRASDESP
                             FROM PCPEDC C, PCPEDI I
                            WHERE C.NUMPED = I.NUMPED
                              AND C.NUMPED = CAB.NUMPED) LOOP
                UPDATE PCMOV
                   SET PUNIT     = ITEM.PVENDA,
                       PUNITCONT = ITEM.PVENDA,
                       ST        = ITEM.ST,
                       BASEICST  = ITEM.BASEICST,
                       vloutros  = ITEM.VLOUTRASDESP
                 WHERE PCMOV.NUMTRANSVENDA = VN_NUMTRANSVENDA
                   AND PCMOV.CODPROD = ITEM.CODPROD
                   AND PCMOV.NUMSEQ = ITEM.NUMSEQ;

                UPDATE PCMOVCOMPLE
                   SET PCMOVCOMPLE.VLBASEOUTROS = COALESCE(ITEM.VLOUTRASDESP, 0)
                 WHERE PCMOVCOMPLE.NUMTRANSITEM = (SELECT PCMOV.NUMTRANSITEM
                                                     FROM PCMOV
                                                    WHERE PCMOV.NUMTRANSVENDA = VN_NUMTRANSVENDA
                                                      AND PCMOV.CODPROD = ITEM.CODPROD
                                                      AND PCMOV.NUMSEQ = ITEM.NUMSEQ);
              END LOOP;

              VNVALORCONT_TOTAL := 0;
              VNVALORTOTALST    := 0;

              -- Obtém os valores do cabeçalho com base nos valores dos itens
              SELECT SUM(VALOR + VALORIPI + VALORST + VALOROUTRAS +
                         VALORFRETE),
                     SUM(VALORST),
                     SUM(VALORBASEST),
                     SUM(VALOROUTRAS)
                INTO VNVALORCONT_TOTAL,
                     VNVALORTOTALST,
                     VNVALORBASEST,
                     VNVALOROUTRAS
                FROM (SELECT SUM(ROUND((NVL(PUNITCONT, 0) -
                                       NVL(PCMOV.VLIPI, 0) -
                                       NVL(PCMOV.ST, 0)) * QTCONT,
                                       2)) VALOR,
                             SUM(ROUND(NVL(PCMOV.VLIPI, 0) * QTCONT, 2)) VALORIPI,
                             SUM(ROUND(NVL(PCMOV.ST, 0) * QTCONT, 2)) VALORST,
                             SUM(ROUND(NVL(PCMOV.BASEICST, 0) * QTCONT, 2)) VALORBASEST,
                             -- 0.107890.2015 - Se for item cesta (tipoitem = 'C'), o cálculo deve ser com o campo "QT"
                             SUM(ROUND(NVL(PCMOV.VLOUTROS, 0) *
                                       (DECODE(PCMOV.TIPOITEM,
                                               'C',
                                               QT,
                                               QTCONT)),
                                       2)) VALOROUTRAS,
                             SUM(ROUND(NVL(PCMOV.VLFRETE, 0) * QTCONT, 2)) VALORFRETE
                        FROM PCMOV
                       WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA
                       GROUP BY CODPROD, NUMSEQ);

              -- Atualiza os valores do cabeçalho da nota sem o boleto
              UPDATE PCNFSAID
                 SET VLTOTAL      = VNVALORCONT_TOTAL,
                     VLTOTGER     = VNVALORCONT_TOTAL,
                     BCST         = VNVALORBASEST,
                     ICMSRETIDO   = VNVALORTOTALST,
                     VLOUTRASDESP = VNVALOROUTRAS
               WHERE NUMTRANSVENDA = VN_NUMTRANSVENDA;

              -- Gerar contas a receber com o novo valor de ST
              vntotalger := VNVALORCONT_TOTAL - vnvloutrasdesp -
                            CAB.VLFRETE;

              -- Verifica novamente se a cobrança é boleto, já que o ROLLBACK da PREST foi executado acima.
              -- Se nao for realizado esse processo e a cobraça antes do ROLLBACK for "CH" ao refazer a prest
              -- a cobrança não era obedecida.
              buscardadosauxiliares(cab.numped,
                                    vsboleto,
                                    vncodsupervisor_usuari,
                                    vnnumregiao_praca,
                                    pvc2menssagen,
                                    P_FORMAPGTOPEDIDO);

              VNQT_CONTASRECEBER := 1;
              GOTO GERAR_CONTASRECEBER; -- Chama o processamento do contas a receber novamente, porém, agora sem o valor do boleto

            END IF;
          END IF;

          vnjuros := 0;
          --Fim Tarefa: 202661
        END IF;

        if (vsCobSupplierCard = 'S') then
          gravarFilaSuppli(cab.planosuppli,
                           cab.condfinanc,
                           ROW_PCNFSAID.NUMTRANSVENDA);
        end if;

        /*IF (vrPARAMETROS.USATXBOLETOAPENASUMANFMESMOCAR = 'S') THEN
          FOR cr IN (SELECT codfilial,
                            codcli,
                            codcob,
                            dtvenc,
                            duplic,
                            prest
                       FROM pcprest
                      WHERE numcar = pncaregamento
                        AND numtransvenda = VN_NUMTRANSVENDA
                        AND codcli = cab.codcli
                        AND vltxboleto > 0) LOOP
            SELECT COUNT(1)
              INTO vicontador
              FROM pcprest
             WHERE numtransvenda <> VN_NUMTRANSVENDA
               AND numcar = pncaregamento
               AND vltxboleto > 0
               AND codcli = cr.codcli
               AND codfilial = cr.codfilial
               AND codcob = cr.codcob
               AND dtvenc = cr.dtvenc;

            IF vicontador > 0 THEN
              -- Retornando o valor da boleta
              SELECT vltxboleto
                INTO vnvltxboleto_prest
                FROM pcprest
               WHERE numtransvenda = VN_NUMTRANSVENDA
                 AND duplic = cr.duplic
                 AND prest = cr.prest;

              -- Retirando o valor da boleta do contas a receber
              FATURAMENTO.vrPARAMETROS.NUMPED  := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA    := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG     := 'Removendo valor de taxa de boleto das parcelas...';

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE TO_CHAR($$PLSQL_LINE) END));

              UPDATE pcprest
                 SET vltxboleto  = 0,
                     valor       = GREATEST(valor -
                                            DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                   'N',
                                                   0,
                                                   vnvltxboleto_prest),
                                            0),
                     valororig   = GREATEST(valororig -
                                            DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                   'N',
                                                   0,
                                                   vnvltxboleto_prest),
                                            0),
                     valorliqcom = GREATEST(valorliqcom -
                                            DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                   'N',
                                                   0,
                                                   vnvltxboleto_prest),
                                            0)
               WHERE numtransvenda = VN_NUMTRANSVENDA
                 AND duplic = cr.duplic
                 AND prest = cr.prest;

              -- Caso esteja somando o boleto nos itens, será estornado
              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Removendo valor de taxa de boleto dos itens...';

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE TO_CHAR($$PLSQL_LINE) END));

              UPDATE pcmov
                 SET vloutros = GREATEST(vloutros -
                                         DECODE(vrPARAMETROS.CON_SOMAVLTARIFAITENSNF,
                                                'N',
                                                0,
                                                vnvltxboleto_prest),
                                         0)
               WHERE numtransvenda = VN_NUMTRANSVENDA
                 AND vloutros > 0;

              -- Retirando o valor do cabeçalho da NF
              FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
              FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
              FATURAMENTO.vrPARAMETROS.MSG    := 'Removendo valor de taxa de boleto do total da NF...';

              FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS, (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE TO_CHAR($$PLSQL_LINE) END));

              UPDATE pcnfsaid
                 SET vloutrasdesp = GREATEST(vloutrasdesp -
                                             DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                    'N',
                                                    0,
                                                    vnvltxboleto_prest),
                                             0),
                     vltotal      = GREATEST(vltotal -
                                             DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                    'N',
                                                    0,
                                                    vnvltxboleto_prest),
                                             0),
                     vltotger     = GREATEST(vltotger -
                                             DECODE(vrPARAMETROS.CON_SOMATARIFABANCDUPLIC,
                                                    'N',
                                                    0,
                                                    vnvltxboleto_prest),
                                             0)
               WHERE numtransvenda = VN_NUMTRANSVENDA;

              vnvltarifa := 0;
            END IF; -- IF vicontador > 0
          END LOOP;
        END IF; -- IF vrPARAMETROS.USATXBOLETOAPENASUMANFMESMOCAR = 'S'
        */

        --Tarefa 56225
        
        FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                  FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                  FATURAMENTO.vrPARAMETROS.MSG    := 'Iniciando gravacao pclanc. valor geracp: ' || cab.geracp;
                  FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                        (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                         '%ORA-%' THEN
                                         DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                         TO_CHAR($$PLSQL_LINE) END));
        IF cab.geracp = 'S' THEN
          BEGIN
            v_numlanc := FERRAMENTAS.F_PROX_RECNUM;

            SELECT pcfilial.codfornec
              INTO vncodfornec
              FROM pcfilial
             WHERE pcfilial.codigo = cab.codfilial;

            INSERT INTO pclanc
              (recnum,
               dtlanc,
               codconta,
               codfornec,
               historico,
               numnota,
               numtransvenda,
               duplic,
               valor,
               dtvenc,
               codfilial,
               indice,
               dtemissao,
               tipoparceiro,
               nomefunc,
               codfornecprinc,
               dtpagto,
               vpago)
            VALUES
              (v_numlanc,
               TRUNC(SYSDATE),
               vrPARAMETROS.CON_CODCONTATRANSFTV10,
               vncodfornec,
               'REF.PED.TRANSFERENCIA ' || cab.numped,
               VN_NUMNOTA,
               VN_NUMTRANSVENDA,
               '1',
               ROW_PCNFSAID.VLTOTGER,
               TRUNC(SYSDATE),
               cab.codfilial,
               'A',
               TRUNC(SYSDATE),
               'F',
               pvc2emitente,
               vncodfornec,
               TRUNC(SYSDATE),
               ROW_PCNFSAID.VLTOTGER);
          EXCEPTION
            WHEN OTHERS THEN
              pvc2menssagen := SQLCODE || '-' || SQLERRM ||
                               ' -0- Erro ao gerar PCLANC. ' || ' - ' ||
                               DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
              VBPULOUPEDIDO := TRUE;
          END;
          FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
                  FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
                  FATURAMENTO.vrPARAMETROS.MSG    := 'Finalizando gravacao pclanc. valor geracp: ' || cab.geracp;
                  FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                                        (CASE WHEN FATURAMENTO.vrPARAMETROS.MSG LIKE
                                         '%ORA-%' THEN
                                         DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                                         TO_CHAR($$PLSQL_LINE) END));          
        END IF;

        -- Tarefa 53399
        -- Caso não tenha sido cobrado Vl. Tarifa Banco na 1ª Parcela é descontado
        IF vbdescboletoparc1 THEN
          IF vnjuros > 0 THEN
            IF vrPARAMETROS.CON_SOMAVLTARIFAITENSNF = 'S' THEN
              UPDATE pcmov
                 SET vloutros = GREATEST(vloutros -
                                         (vnjuros /
                                         DECODE(NVL(qtcont, 0),
                                                 0,
                                                 qt,
                                                 qtcont)),
                                         0)
               WHERE numtransvenda = VN_NUMTRANSVENDA
                 AND vloutros > 0
                 AND ROWNUM = 1;
            END IF;

            IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S' THEN
              UPDATE pcnfsaid
                 SET vltotal      = DECODE(NVL(vltotal, 0),
                                           0,
                                           0,
                                           (NVL(vltotal, 0) - vnjuros)),
                     vltotger     = NVL(vltotger, 0) - vnjuros,
                     vloutrasdesp = NVL(vloutrasdesp, 0) - vnjuros,
                     vltabela     = NVL(vltabela, 0) - vnjuros
               WHERE numtransvenda = VN_NUMTRANSVENDA;
            END IF;
            -- IF vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S'
          END IF; -- IF vnjuros > 0
        END IF; -- IF vbDescBoletoParc1

        -- Tarefa 53399

        -- Verifica se houve diferenca entre PCPREST e PCMOV --
        SELECT SUM(DECODE(status, 'A', NVL(valor, 0), 0))
          INTO vntotalcontasrecebercont
          FROM pcprest
         WHERE numtransvenda = VN_NUMTRANSVENDA
           AND dtpag IS NULL;

        -- Tarefa 26594
        -- Tarefa 67118
        SELECT SUM(DECODE(cab.condvenda,
                          5,
                          ROUND(NVL(qt, 0) * (NVL(punitcont, 0)), 2),
                          6,
                          ROUND(NVL(qt, 0) *
                                (NVL(pbonific, NVL(ptabela, 0))),
                                2),
                          11,
                          ROUND(NVL(qt, 0) * (NVL(punitcont, 0)), 2),
                          ROUND(NVL(qt, 0) * (NVL(punit, 0)), 2))),
               SUM(ROUND(DECODE(cab.especiemanif,
                                'NF',
                                NVL(qtcont, 0),
                                NVL(qt, 0)) * (NVL(punitcont, 0)),
                         2)),
               SUM(ROUND(NVL(qt, 0) * NVL(vloutros, 0), 2)),
               SUM(ROUND(DECODE(cab.especiemanif,
                                'NF',
                                NVL(qtcont, 0),
                                NVL(qt, 0)) * NVL(vloutros, 0),
                         2)),
               SUM(ROUND(qtcont * vldesconto, 2))
          INTO vntotalitens,
               vntotalitenscont,
               vnvloutros,
               vnvloutroscont,
               vntotvldesconto
          FROM pcmov
         WHERE numtransvenda = VN_NUMTRANSVENDA;

        -- Tarefa 67118

        -- Tarefa 74614
        vntotalitens     := vntotalitens + vnvloutros;
        vntotalitenscont := vntotalitenscont + vnvloutroscont;
        -- Fim tarefa 74614
        vntotalitens     := ROUND(GREATEST((NVL(vntotalitens, 0) -
                                           NVL(vncredito, 0)),
                                           0) + NVL(cab.vloutrasdesp, 0),
                                  2);
        vntotalitenscont := ROUND(GREATEST((NVL(vntotalitenscont, 0) -
                                           (NVL(vncredito, 0) * vnindicea)),
                                           0) + NVL(cab.vloutrasdesp, 0),
                                  2);

        -- Tarefa 35452
        IF vrPARAMETROS.CON_SOMAVLTARIFAITENSNF = 'S' THEN
          vntotalitens     := vntotalitens -
                              (visomatarifanf * NVL(vnvltarifa, 0));
          vntotalitenscont := vntotalitenscont -
                              (visomatarifanf * NVL(vnvltarifa, 0));
        END IF;

        -- Fim tarefa 35452

        -- Tarefa 47656
        IF cab.conciliaimportacao = 'S' THEN
          vntotalger := 0;

          SELECT ROUND(SUM((pcpedi.pvenda - pclote.precocompra) * pcpedi.qt),
                       2)
            INTO vntotalitens
            FROM pcpedi, pclote
           WHERE pcpedi.numped = cab.numped
             AND pcpedi.codprod = pclote.codprod
             AND pcpedi.numlote = pclote.numlote
             AND NVL(pcpedi.codfilialretira, cab.codfilial) =
                 pclote.codfilial
             AND pcpedi.pvenda > pclote.precocompra;

          vntotalitenscont := ROUND((vntotalitens), 2);
        END IF;

        -- Fim Tarefa 47656
        /*Tarefa: 204694
        vndifercont := NVL(vntotalcontasrecebercont, 0) -
                       NVL(cab.vlfrete, 0) -
                       NVL(vntotalitenscont, 0) -
                       (visomatarifanf * NVL(vnvltarifa, 0));*/

        IF (vnindicea > 0) AND (vndifercont <> 0) THEN
          vnvalorprest1 := 0;

          SELECT COUNT(1)
            INTO vicontador
            FROM pcprest
           WHERE numtransvenda = VN_NUMTRANSVENDA
             AND status = 'A'
             AND dtpag IS NULL
             AND ROWNUM = 1;

          IF vicontador = 0 THEN
            vnvalorprest1 := 0;
          ELSE
            SELECT valor
              INTO vnvalorprest1
              FROM pcprest
             WHERE numtransvenda = VN_NUMTRANSVENDA
               AND status = 'A'
               AND dtpag IS NULL
               AND ROWNUM = 1;
            /*
            IF (vndifercont <> vnvalorPrest1)
            THEN
              UPDATE pcprest
                 SET valor       = nvl(valor, 0) - vndifercont,
                     valororig   = nvl(valororig, 0) - vndifercont,
                     valorliqcom = nvl(valorliqcom, 0) - vndifercont,
                     valordesc   = (nvl(valor, 0) - vndifercont) *
                                   (vnperdescfin_client / 100)
               WHERE numtransvenda = VN_NUMTRANSVENDA
                 AND status = 'A'
                 AND DTPAG IS NULL
                 AND ((cab.vlentrada > 0) AND (codcob <> 'DH')) -- Tarefa 103223
                 AND rownum = 1;
            END IF;
            */
          END IF; -- IF vicontador = 0 THEN
        END IF;

        SELECT COUNT(1)
          INTO vicontador
          FROM pcprest
         WHERE numtransvenda = VN_NUMTRANSVENDA
           AND dtpag IS NULL;

        IF vicontador = 0 THEN
          vntotalcontasreceber := 0;
        ELSE
          SELECT SUM(NVL(valor, 0))
            INTO vntotalcontasreceber
            FROM pcprest
           WHERE numtransvenda = VN_NUMTRANSVENDA
             AND dtpag IS NULL;
        END IF;

        vndifer := NVL(vntotalcontasreceber, 0) - NVL(cab.vlfrete, 0) -
                   NVL(vntotalitens, 0) -
                   (visomatarifanf * NVL(vnvltarifa, 0));

        IF ((vnindiceb > 0) AND (vndifer <> 0)) THEN
          vnvalorprest2 := 0;

          SELECT valor
            INTO vnvalorprest2
            FROM pcprest
           WHERE numtransvenda = VN_NUMTRANSVENDA
             AND status = 'B'
             AND dtpag IS NULL
             AND ROWNUM = 1;

          IF (vndifer <> vnvalorprest2) THEN
            UPDATE pcprest
               SET valor       = NVL(valor, 0) - vndifer,
                   valororig   = NVL(valororig, 0) - vndifer,
                   valorliqcom = NVL(valorliqcom, 0) - vndifer,
                   valordesc  =
                   (NVL(valor, 0) - vndifer) * (vnperdescfin_client / 100)
             WHERE numtransvenda = VN_NUMTRANSVENDA
               AND status = 'B'
               AND dtpag IS NULL
               AND ROWNUM = 1;
          END IF;
        END IF;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        pvc2menssagen                   := SQLCODE || '-' || SQLERRM ||
                                           CHR(13) || '22- Geracao do CR.' ||
                                           CHR(13) ||
                                           DBMS_UTILITY.format_error_backtrace || -- Deyvid Costa - Retorna a linha que gerou a exceção
                                           CHR(13);
        FATURAMENTO.vrPARAMETROS.NUMPED := cab.numped;
        FATURAMENTO.vrPARAMETROS.DATA   := SYSDATE;
        FATURAMENTO.vrPARAMETROS.MSG    := pvc2menssagen;

        FATURAMENTO.GRAVARLOG(FATURAMENTO.vrPARAMETROS,
                              (CASE WHEN
                               FATURAMENTO.vrPARAMETROS.MSG LIKE '%ORA-%' THEN
                               DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ELSE
                               TO_CHAR($$PLSQL_LINE) END));
        RAISE;
    END; -- Gravacao do CR

    --Tarefa 18634
    BEGIN
      IF (vsformaparcelamento_plpag = 'C') THEN
        vnprazomedio := TRUNC(vnprazomedio / vnnumparc);
      ELSIF (vsformaparcelamento_plpag = 'T') THEN
        vnprazomedio := vendas.RETORNA_PRAZOMEDIDO_PLPAGFIXO(CAB.CODPLPAG);
      ELSE
        vnPrazoMedio := CAB.PRAZOMEDIO;
      END IF;

      UPDATE pcnfsaid
         SET prazoadicional = vnprazoadicional_cli,
             prazomedio     = vnprazomedio, -- TRUNC(vnprazomedio / vnnumparc),
             vldesconto     = vntotvldesconto -- Tarefa 149894
       WHERE numtransvenda = VN_NUMTRANSVENDA;
    EXCEPTION
      WHEN OTHERS THEN
        pvc2menssagen := SQLCODE || '-' || SQLERRM || CHR(13) ||
                         ' -22.1- Atualizacao do prazo da NF.' || CHR(13) ||
                         ' - ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
        RAISE;
    END;



    ----------------------------------------------------------------------------------------

    SELECT VLTOTGER
      INTO vntotalger
      FROM PCNFSAID
     WHERE NUMTRANSVENDA = ROW_PCNFSAID.NUMTRANSVENDA;

    R_DADOS.VLTOTGER    := vntotalger;
    R_DADOS.PULOUPEDIDO := VBPULOUPEDIDO;
    R_DADOS.MENSAGEM    := pvc2menssagen;

    --PIPE ROW(R_DADOS);
    RETURN R_DADOS;
  EXCEPTION
    WHEN ERRO_GERAL THEN
      R_DADOS          := NULL;
      R_DADOS.MENSAGEM := pvc2menssagen;
      -- PIPE ROW(R_DADOS);
      RETURN R_DADOS;
    WHEN ERRO_CONTASARECEBER THEN
      R_DADOS          := NULL;
      R_DADOS.MENSAGEM := pvc2menssagen;
      -- PIPE ROW(R_DADOS);
      RETURN R_DADOS;
    WHEN OTHERS THEN
      R_DADOS          := NULL;
      R_DADOS.MENSAGEM := pvc2menssagen;
      -- PIPE ROW(R_DADOS);
      RETURN R_DADOS;
  END GERARFINANCEIRO;

  FUNCTION gerarFinanMultCob(CAB                IN FATURAMENTO.C_PEDIDOS%ROWTYPE,
                             vrPARAMETROS       IN PCPARAMFAT%ROWTYPE,
                            pvc2emitente       IN VARCHAR2)
   RETURN T_GERARFINANCEIRO_DT IS

    -- INÍCIO - Cursor para processar os financeiros para os planos de pagamentos cadastrados na tabela PCFORMAPGTOPEDIDO ---------------
    CURSOR C_FORMAPGTOPEDIDO(C_NUMPED PCPEDC.NUMPED%TYPE, C_ORDEM INTEGER) IS
      SELECT S.*
        FROM PCFORMAPGTOPEDIDO S, PCCOB C
       WHERE S.NUMPED = C_NUMPED
         AND S.CODCOB = C.CODCOB(+)
         AND (NVL(C.CODCOBSEFAZ, 99) = DECODE(C_ORDEM, 1, 02, NVL(C.CODCOBSEFAZ, 99)) OR     -- 2348.112652.2018
             NVL(C.CODCOBSEFAZ, 99) = DECODE(C_ORDEM, 1, 03, NVL(C.CODCOBSEFAZ, 99)) OR
             NVL(C.CODCOBSEFAZ, 99) = DECODE(C_ORDEM, 1, 04, NVL(C.CODCOBSEFAZ, 99)) OR
             NVL(C.CODCOBSEFAZ, 99) = DECODE(C_ORDEM, 1, 15, NVL(C.CODCOBSEFAZ, 99)) OR
             NVL(C.CODCOBSEFAZ, 99) = DECODE(C_ORDEM, 1, 99, NVL(C.CODCOBSEFAZ, 99))) --IN (02, 03, 04, 15, 99)
         AND (NVL(C.CODCOBSEFAZ, 99) <> DECODE(C_ORDEM, 2, 02, -1) AND
             NVL(C.CODCOBSEFAZ, 99) <> DECODE(C_ORDEM, 2, 03, -1) AND
             NVL(C.CODCOBSEFAZ, 99) <> DECODE(C_ORDEM, 2, 04, -1) AND
             NVL(C.CODCOBSEFAZ, 99) <> DECODE(C_ORDEM, 2, 15, -1) AND
             NVL(C.CODCOBSEFAZ, 99) <> DECODE(C_ORDEM, 2, 99, -1))
       ORDER BY VALOR;

    VALORCURSOR     C_FORMAPGTOPEDIDO%ROWTYPE; -- Variável contendo tipo do cursor
    VNNUMTRANSVENDA PCNFSAID.NUMTRANSVENDA%TYPE;
    VNVALORTOTALNF  PCNFSAID.VLTOTGER%TYPE;
    VNTOTALCOBPED   PCNFSAID.VLTOTGER%TYPE;
    VSCODCOBDIF     PCNFSAID.CODCOB%TYPE;
    VBENCONTROU     BOOLEAN := FALSE;

    R_DADOSFINAN PKG_GERAFINANFATURAMENTO_FT51.T_GERARFINANCEIRO_DT;

  begin
    BEGIN
      SELECT NUMTRANSVENDA, VLTOTGER INTO VNNUMTRANSVENDA, VNVALORTOTALNF
        FROM PCNFSAID S
       WHERE NUMPED = CAB.Numped;
    EXCEPTION
      WHEN OTHERS THEN
        VNNUMTRANSVENDA := 0;
        VNVALORTOTALNF  := 0;
    END;

    BEGIN
      VNTOTALCOBPED := 0;

      FOR DADOS IN (SELECT C.*
                      FROM PCFORMAPGTOPEDIDO C
                     WHERE C.NUMPED = CAB.NUMPED
                           AND C.CODCOB <> 'BNF')
      LOOP
        VNTOTALCOBPED := VNTOTALCOBPED + NVL(DADOS.VALOR,0) - NVL(DADOS.VLRTROCO, 0) - NVL(DADOS.VLENTRADA, 0);
        IF (vrPARAMETROS.CON_SOMATARIFABANCDUPLIC = 'S') THEN
          VNTOTALCOBPED := VNTOTALCOBPED + FATURAMENTO.VALORTARIFA(CAB.NUMPED, CAB.VLATEND, DADOS);
        END IF;
      END LOOP;

      /*
      SELECT SUM(A.VALOR) - SUM(NVL(A.VLRTROCO, 0)) - SUM(NVL(A.VLENTRADA, 0))
        INTO VNTOTALCOBPED
        FROM PCFORMAPGTOPEDIDO A
       WHERE A.NUMPED = CAB.NUMPED;
     */
      VSCODCOBDIF := F_RETORNAR_COB_ACRES_DIFER(CAB.NUMPED);

    EXCEPTION
      WHEN OTHERS THEN
        VNTOTALCOBPED := 0;
    END;

    IF VNNUMTRANSVENDA > 0 THEN
      DECLARE
        PROCEDURE PROCESSAFINAN IS
        BEGIN
          -------------------------------- ALTERAR VALOR DA COBRANÇA PARA JOGAR A DIFERENÇA, CASO EXISTA ---------------------------------
          IF (VNTOTALCOBPED <> VNVALORTOTALNF) AND
             (VSCODCOBDIF = VALORCURSOR.CODCOB)
          THEN
            VBENCONTROU := TRUE;
            VALORCURSOR.VALOR := VALORCURSOR.VALOR + (NVL(VNVALORTOTALNF, 0) - NVL(VNTOTALCOBPED, 0));
          END IF;
          -------------------------------- ALTERAR VALOR DA COBRANÇA PARA JOGAR A DIFERENÇA, CASO EXISTA ---------------------------------

          R_DADOSFINAN := PKG_GERAFINANFATURAMENTO_FT51.GERARFINANCEIRO(VNNUMTRANSVENDA,
                                                                   CAB,
                                                                   vrPARAMETROS,
                                                                   VALORCURSOR,
                                                                   pvc2emitente);
        END;
      BEGIN
        -- 2348.112652.2018
        OPEN C_FORMAPGTOPEDIDO(CAB.Numped, 1); -- RETORNA APENAS OS REGISTROS COM CODCOBSEFAZ = (02, 03, 04, 15, 99)
        LOOP
          FETCH C_FORMAPGTOPEDIDO
            INTO VALORCURSOR;
          EXIT WHEN C_FORMAPGTOPEDIDO%NOTFOUND;

            PROCESSAFINAN;
        END LOOP;
        CLOSE C_FORMAPGTOPEDIDO;
        --------------- FIM DO LOOP

        OPEN C_FORMAPGTOPEDIDO(CAB.Numped, 2); -- RETORNA APENAS OS REGISTROS COM CODCOBSEFAZ <> (02, 03, 04, 15, 99)
        LOOP
          FETCH C_FORMAPGTOPEDIDO
            INTO VALORCURSOR;
          EXIT WHEN C_FORMAPGTOPEDIDO%NOTFOUND;

            PROCESSAFINAN;
        END LOOP;
        CLOSE C_FORMAPGTOPEDIDO;
        --------------- FIM DO LOOP
      END;

      -- GERANDO A PARCELA CASO NÃO EXISTE';
      IF NOT VBENCONTROU THEN
        VALORCURSOR.CODCOB := CASE FERRAMENTAS.F_BUSCARPARAMETRO_ALFA('CODCOBDIFERFINANFATUR', '99', 'D')
                               WHEN 'D' THEN
                                 'D'
                               WHEN 'C' THEN
                                 'CH'
                               WHEN 'B' THEN
                                 'BK'
                               END;
        VALORCURSOR.VALOR             := (NVL(VNVALORTOTALNF, 0) - NVL(VNTOTALCOBPED, 0));

        VALORCURSOR.DTVENC            := TRUNC(SYSDATE);
        VALORCURSOR.CODPLPAG          := 1;
        VALORCURSOR.TIPOINTEGRACAO    := NULL;
        VALORCURSOR.CNPJCREDCARTAO    := NULL;
        VALORCURSOR.NUMAUTORIZACAO    := NULL;
        VALORCURSOR.VLRTROCO          := 0;
        VALORCURSOR.CODBANDEIRACARTAO := NULL;
        VALORCURSOR.VLENTRADA         := 0;

        R_DADOSFINAN := PKG_GERAFINANFATURAMENTO_FT51.GERARFINANCEIRO(VNNUMTRANSVENDA,
                                                               CAB,
                                                               vrPARAMETROS,
                                                               VALORCURSOR,
                                                               pvc2emitente);
      END IF;
    ELSE
        R_DADOSFINAN := PKG_GERAFINANFATURAMENTO_FT51.GERARFINANCEIRO(VNNUMTRANSVENDA,
                                                                 CAB,
                                                                 vrPARAMETROS,
                                                                 NULL,
                                                                 pvc2emitente);

    END IF;


    RETURN R_DADOSFINAN;
  end gerarFinanMultCob;


END;
/



-- End of DDL Script for Package Body TESTE.PKG_GERAFINANFATURAMENTO_FT51

